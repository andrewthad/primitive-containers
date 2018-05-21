{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}
module Data.Set.Internal
  ( Set(..)
  , empty
  , singleton
  , append
  , member
  , showsPrec
  , equals
  , compare
  , fromListN
  , fromList
  , toList
  , size
  , concat
    -- * Folds
  , foldr
  , foldl'
  , foldr'
  , foldMap'
  ) where

import Prelude hiding (compare,showsPrec,concat,foldr)
import qualified Prelude as P

import Control.Monad.ST (ST,runST)
import Data.Primitive.UnliftedArray (PrimUnlifted(..))
import Data.Internal (Contiguous,Mutable,Element)
import qualified Data.Foldable as F
import qualified Data.Internal as A

newtype Set arr a = Set (arr a)

instance Contiguous arr => PrimUnlifted (Set arr a) where
  toArrayArray# (Set a) = A.unlift a
  fromArrayArray# a = Set (A.lift a)

append :: (Contiguous arr, Element arr a, Ord a) => Set arr a -> Set arr a -> Set arr a
append (Set x) (Set y) = Set (unionArr x y)
  
empty :: Contiguous arr => Set arr a
empty = Set A.empty

equals :: (Contiguous arr, Element arr a, Eq a) => Set arr a -> Set arr a -> Bool
equals (Set x) (Set y) = A.equals x y

compare :: (Contiguous arr, Element arr a, Ord a) => Set arr a -> Set arr a -> Ordering
compare (Set x) (Set y) = compareArr x y

fromListN :: (Contiguous arr, Element arr a, Ord a) => Int -> [a] -> Set arr a
fromListN n xs = -- fromList xs
  case xs of
    [] -> empty
    y : ys ->
      let (leftovers, result) = fromAscList (max 1 n) y ys
       in concat (result : P.map singleton leftovers)

fromList :: (Contiguous arr, Element arr a, Ord a) => [a] -> Set arr a
fromList = fromListN 1

fromAscList :: forall arr a. (Contiguous arr, Element arr a, Ord a)
  => Int -- initial size of buffer, must be 1 or higher
  -> a -- first element
  -> [a] -- elements
  -> ([a], Set arr a)
fromAscList !n x0 xs0 = runST $ do
  marr0 <- A.new n
  A.write marr0 0 x0
  let go :: forall s. Int -> a -> Int -> Mutable arr s a -> [a] -> ST s ([a], Set arr a)
      go !ix !_ !sz !marr [] = if ix == sz
        then do
          arr <- A.unsafeFreeze marr
          return ([],Set arr)
        else do
          marr' <- A.resize marr ix
          arr <- A.unsafeFreeze marr'
          return ([],Set arr)
      go !ix !old !sz !marr (x : xs) = if ix < sz
        then case P.compare x old of
          GT -> do
            A.write marr ix x
            go (ix + 1) x sz marr xs
          EQ -> go ix x sz marr xs
          LT -> do
            marr' <- A.resize marr ix
            arr <- A.unsafeFreeze marr'
            return (x : xs,Set arr)
        else do
          let sz' = sz * 2
          marr' <- A.resize marr sz'
          go ix old sz' marr' (x : xs)
  go 1 x0 n marr0 xs0

showsPrec :: (Contiguous arr, Element arr a, Show a) => Int -> Set arr a -> ShowS
showsPrec p xs = showParen (p > 10) $
  showString "fromList " . shows (toList xs)

toList :: (Contiguous arr, Element arr a) => Set arr a -> [a]
toList = foldr (:) []

member :: forall arr a. (Contiguous arr, Element arr a, Ord a) => a -> Set arr a -> Bool
member a (Set arr) = go 0 (A.size arr - 1) where
  go :: Int -> Int -> Bool
  go !start !end = if end < start
    then False
    else
      let !mid = div (end + start) 2
          !v = A.index arr mid
       in case P.compare a v of
            LT -> go start (mid - 1)
            EQ -> True
            GT -> go (mid + 1) end
{-# INLINEABLE member #-}

concat :: forall arr a. (Contiguous arr, Element arr a, Ord a) => [Set arr a] -> Set arr a
concat = go [] where
  go :: [Set arr a] -> [Set arr a] -> Set arr a
  go !stack [] = F.foldl' append empty stack
  go !stack (x : xs) = if size x > 0
    then go (pushStack x stack) xs
    else go stack xs
  pushStack :: Set arr a -> [Set arr a] -> [Set arr a]
  pushStack x [] = [x]
  pushStack x (s : ss) = if size x >= size s
    then pushStack (append x s) ss
    else x : s : ss

compareArr :: (Contiguous arr, Element arr a, Ord a)
  => arr a
  -> arr a
  -> Ordering
compareArr arrA arrB = go 0 where
  go :: Int -> Ordering
  go !ix = if ix < A.size arrA
    then if ix < A.size arrB
      then mappend (P.compare (A.index arrA ix) (A.index arrB ix)) (go (ix + 1))
      else GT
    else if ix < A.size arrB
      then LT
      else EQ

singleton :: (Contiguous arr, Element arr a) => a -> Set arr a
singleton a = Set $ runST $ do
  arr <- A.new 1
  A.write arr 0 a
  A.unsafeFreeze arr

unionArr :: forall arr a. (Contiguous arr, Element arr a, Ord a)
  => arr a -- array x
  -> arr a -- array y
  -> arr a
unionArr arrA arrB
  | szA < 1 = arrB
  | szB < 1 = arrA
  | otherwise = runST $ do
      !(arrDst :: Mutable arr s a)  <- A.new (szA + szB)
      let go !ixA !ixB !ixDst = if ixA < szA
            then if ixB < szB
              then do
                let !a = A.index arrA ixA
                    !b = A.index arrB ixB
                case P.compare a b of
                  EQ -> do
                    A.write arrDst ixDst a
                    go (ixA + 1) (ixB + 1) (ixDst + 1)
                  LT -> do
                    A.write arrDst ixDst a
                    go (ixA + 1) ixB (ixDst + 1)
                  GT -> do
                    A.write arrDst ixDst b
                    go ixA (ixB + 1) (ixDst + 1)
              else do
                A.copy arrDst ixDst arrA ixA (szA - ixA)
                return (ixDst + (szA - ixA))
            else if ixB < szB
              then do
                A.copy arrDst ixDst arrB ixB (szB - ixB)
                return (ixDst + (szB - ixB))
              else return ixDst
      total <- go 0 0 0
      arrFinal <- A.resize arrDst total
      A.unsafeFreeze arrFinal
  where
  !szA = A.size arrA
  !szB = A.size arrB

size :: (Contiguous arr, Element arr a) => Set arr a -> Int
size (Set arr) = A.size arr

foldr :: (Contiguous arr, Element arr a)
  => (a -> b -> b)
  -> b
  -> Set arr a
  -> b
foldr f b0 (Set arr) = A.foldr f b0 arr
{-# INLINEABLE foldr #-}

foldl' :: (Contiguous arr, Element arr a)
  => (b -> a -> b)
  -> b
  -> Set arr a
  -> b
foldl' f b0 (Set arr) = A.foldl' f b0 arr
{-# INLINEABLE foldl' #-}

foldr' :: (Contiguous arr, Element arr a)
  => (a -> b -> b)
  -> b
  -> Set arr a
  -> b
foldr' f b0 (Set arr) = A.foldr' f b0 arr
{-# INLINEABLE foldr' #-}

foldMap' :: (Contiguous arr, Element arr a, Monoid m)
  => (a -> m)
  -> Set arr a
  -> m
foldMap' f (Set arr) = A.foldMap' f arr
{-# INLINEABLE foldMap' #-}
