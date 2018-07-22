{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}
module Data.Diet.Set.Unboxed
  ( Set(..)
  , singleton
  , member
  , difference
  , intersection
    -- * Split
  , aboveInclusive
  , belowInclusive
  , betweenInclusive
    -- * Folds
  , foldr
    -- * List Conversion
  , toList
  , fromList
  , fromListN
  ) where

import Prelude hiding (lookup,map,foldr)

import Data.Semigroup (Semigroup)
import Data.Functor.Classes (Show2(..))
import Data.Primitive.Types (Prim)
import Data.Primitive.PrimArray (PrimArray)
import qualified GHC.Exts as E
import qualified Data.Semigroup as SG
import qualified Data.Diet.Set.Internal as I

-- | A diet set. Currently, the data constructor for this type is
-- exported. Please do not use it.
newtype Set a = Set (I.Set PrimArray a)

-- | /O(1)/ Create a diet set with a single element.
singleton :: (Ord a, Prim a)
  => a -- ^ inclusive lower bound
  -> a -- ^ inclusive upper bound
  -> Set a
singleton lo hi = Set (I.singleton lo hi)

-- | /O(log n)/ Lookup the value at a key in the map.
member :: (Ord a, Prim a) => a -> Set a -> Bool
member a (Set s) = I.member a s

instance (Show a, Prim a) => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

instance (Eq a, Prim a) => Eq (Set a) where
  Set x == Set y = I.equals x y

instance (Ord a, Prim a) => Ord (Set a) where
  compare (Set xs) (Set ys) = compare (I.toList xs) (I.toList ys)

instance (Ord a, Enum a, Prim a) => Semigroup (Set a) where
  Set x <> Set y = Set (I.append x y)

instance (Ord a, Enum a, Prim a) => Monoid (Set a) where
  mempty = Set I.empty
  mappend = (SG.<>)
  mconcat = Set . I.concat . E.coerce

instance (Ord a, Enum a, Prim a) => E.IsList (Set a) where
  type Item (Set a) = (a,a)
  fromListN n = Set . I.fromListN n
  fromList = Set . I.fromList
  toList (Set s) = I.toList s

toList :: Prim a => Set a -> [(a,a)]
toList (Set x) = I.toList x

fromList :: (Ord a, Enum a, Prim a) => [(a,a)] -> Set a
fromList = Set . I.fromList

fromListN :: (Ord a, Enum a, Prim a)
  => Int -- ^ expected size of resulting diet 'Set'
  -> [(a,a)] -- ^ key-value pairs
  -> Set a
fromListN n = Set . I.fromListN n

-- | /O(n + m*log n)/ Subtract the subtrahend of size @m@ from the
-- minuend of size @n@. It should be possible to improve the improve
-- the performance of this to /O(n + m)/. Anyone interested in doing
-- this should open a PR.
difference :: (Ord a, Enum a, Prim a)
  => Set a -- ^ minuend
  -> Set a -- ^ subtrahend
  -> Set a
difference (Set x) (Set y) = Set (I.difference x y)

-- | The intersection of two diet sets.
intersection :: (Ord a, Enum a, Prim a)
  => Set a -- ^ minuend
  -> Set a -- ^ subtrahend
  -> Set a
intersection (Set x) (Set y) = Set (I.intersection x y)

foldr :: Prim a => (a -> a -> b -> b) -> b -> Set a -> b
foldr f z (Set arr) = I.foldr f z arr

-- | /O(n)/ The subset where all elements are greater than
-- or equal to the given value. 
aboveInclusive :: (Ord a, Prim a)
  => a -- ^ inclusive lower bound
  -> Set a
  -> Set a
aboveInclusive x (Set s) = Set (I.aboveInclusive x s)

-- | /O(n)/ The subset where all elements are less than
-- or equal to the given value. 
belowInclusive :: (Ord a, Prim a)
  => a -- ^ inclusive upper bound
  -> Set a
  -> Set a
belowInclusive x (Set s) = Set (I.belowInclusive x s)

-- | /O(n)/ The subset where all elements are greater than
-- or equal to the lower bound and less than or equal to
-- the upper bound.
betweenInclusive :: (Ord a, Prim a)
  => a -- ^ inclusive lower bound
  -> a -- ^ inclusive upper bound
  -> Set a
  -> Set a
betweenInclusive x y (Set s) = Set (I.betweenInclusive x y s)

