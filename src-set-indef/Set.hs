{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
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
  ) where

import Prelude hiding (compare,showsPrec)
import qualified Prelude as P

import Value (Arr,Ctx)
import Data.Semigroup (Semigroup)
import Control.Monad.ST (ST,runST)
import qualified Value as A
import qualified Data.Semigroup as SG
import qualified GHC.Exts as E

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
fromListN _ = foldr (\a acc -> append (singleton a) acc) empty

fromList :: (Ctx a, Ord a) => [a] -> Set a
fromList = foldr (\a acc -> append (singleton a) acc) empty

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
unionArr arrA arrB = runST $ do
  let !szA = A.size arrA
      !szB = A.size arrB
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




