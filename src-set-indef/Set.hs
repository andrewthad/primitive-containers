{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}
module Set
  ( Set
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
  ) where

import Prelude hiding (compare,showsPrec,concat)
import qualified Prelude as P

import Value (Arr,Ctx)
import Control.Monad.ST (ST,runST)
import Data.Foldable (foldl')
import qualified Value as A

newtype Set a = Set (Arr a)

append :: (Ctx a, Ord a) => Set a -> Set a -> Set a
append (Set x) (Set y) = Set (unionArr x y)
  
empty :: Ctx a => Set a
empty = Set (runST (A.new 0 >>= A.unsafeFreeze))

equals :: (Ctx a, Eq a) => Set a -> Set a -> Bool
equals (Set x) (Set y) = x == y

compare :: (Ctx a, Ord a) => Set a -> Set a -> Ordering
compare (Set x) (Set y) = compareArr x y

fromListN :: (Ctx a, Ord a) => Int -> [a] -> Set a
fromListN n xs = -- fromList xs
  case xs of
    [] -> empty
    y : ys ->
      let (leftovers, result) = fromAscList (max 1 n) y ys
       in concat (result : P.map singleton leftovers)

fromList :: (Ctx a, Ord a) => [a] -> Set a
fromList xs = -- concat (P.map singleton xs)
  case xs of
    [] -> empty
    y : ys ->
      let (leftovers, result) = fromAscList 1 y ys
       in concat (result : P.map singleton leftovers)

fromAscList :: forall a. (Ctx a, Ord a)
  => Int -- initial size of buffer, must be 1 or higher
  -> a -- first element
  -> [a] -- elements
  -> ([a], Set a)
fromAscList !n x0 xs0 = runST $ do
  marr0 <- A.new n
  A.write marr0 0 x0
  let go :: forall s. Int -> a -> Int -> A.MArr s a -> [a] -> ST s ([a], Set a)
      go !ix !_ !sz !marr [] = if ix == sz
        then do
          arr <- A.unsafeFreeze marr
          return ([],Set arr)
        else do
          marr' <- A.resize marr ix
          arr <- A.unsafeFreeze marr'
          return ([],Set arr)
      go !ix !old !sz !marr (x : xs) = if ix < sz
        then do
          case P.compare x old of
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

showsPrec :: (Ctx a, Show a) => Int -> Set a -> ShowS
showsPrec p xs = showParen (p > 10) $
  showString "fromList " . shows (toList xs)

toList :: Ctx a => Set a -> [a]
toList (Set arr) = A.foldr (:) [] arr

member :: forall a. (Ctx a, Ord a) => a -> Set a -> Bool
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

concat :: forall a. (Ctx a, Ord a) => [Set a] -> Set a
concat = go [] where
  go :: [Set a] -> [Set a] -> Set a
  go !stack [] = foldl' append empty stack
  go !stack (x : xs) = if size x > 0
    then go (pushStack x stack) xs
    else go stack xs
  pushStack :: Set a -> [Set a] -> [Set a]
  pushStack x [] = [x]
  pushStack x (s : ss) = if size x >= size s
    then pushStack (append x s) ss
    else x : s : ss

compareArr :: forall a. (Ctx a, Ord a)
  => Arr a
  -> Arr a
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

singleton :: Ctx a => a -> Set a
singleton a = Set $ runST $ do
  arr <- A.new 1
  A.write arr 0 a
  A.unsafeFreeze arr

unionArr :: (Ctx a, Ord a)
  => Arr a -- array x
  -> Arr a -- array y
  -> Arr a
unionArr arrA arrB
  | szA < 1 = arrB
  | szB < 1 = arrA
  | otherwise = runST $ do
      arrDst <- A.new (szA + szB)
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

size :: Ctx a => Set a -> Int
size (Set arr) = A.size arr


