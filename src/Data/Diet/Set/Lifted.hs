{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}
module Data.Diet.Set.Lifted
  ( Set(..)
  , singleton
  , member
  , difference
  , intersection
  , negate
    -- * Split
  , aboveInclusive
  , belowInclusive
  , betweenInclusive
    -- * Folds
  , foldr
    -- * List Conversion
  , fromList
  , fromListN
  ) where

import Prelude hiding (lookup,map,foldr,negate)

import Data.Semigroup (Semigroup)
import Data.Primitive (Array)
import qualified GHC.Exts as E
import qualified Data.Semigroup as SG
import qualified Data.Diet.Set.Internal as I

-- | A diet set. Currently, the data constructor for this type is
-- exported. Please do not use it. It will be moved to an internal
-- module at some point.
newtype Set a = Set (I.Set Array a)

-- | /O(1)/ Create a diet set with a single element.
singleton :: Ord a
  => a -- ^ inclusive lower bound
  -> a -- ^ inclusive upper bound
  -> Set a
singleton lo hi = Set (I.singleton lo hi)

-- | /O(log n)/ Returns @True@ if the element is a member of the diet set.
member :: Ord a => a -> Set a -> Bool
member a (Set s) = I.member a s

instance Show a => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

instance Eq a => Eq (Set a) where
  Set x == Set y = I.equals x y

instance Ord a => Ord (Set a) where
  compare (Set xs) (Set ys) = compare (I.toList xs) (I.toList ys)

instance (Ord a, Enum a) => Semigroup (Set a) where
  Set x <> Set y = Set (I.append x y)

instance (Ord a, Enum a) => Monoid (Set a) where
  mempty = Set I.empty
  mappend = (SG.<>)
  mconcat = Set . I.concat . E.coerce

instance (Ord a, Enum a) => E.IsList (Set a) where
  type Item (Set a) = (a,a)
  fromListN n = Set . I.fromListN n
  fromList = Set . I.fromList
  toList (Set s) = I.toList s

fromList :: (Ord a, Enum a) => [(a,a)] -> Set a
fromList = Set . I.fromList

fromListN :: (Ord a, Enum a)
  => Int -- ^ expected size of resulting diet 'Set'
  -> [(a,a)] -- ^ key-value pairs
  -> Set a
fromListN n = Set . I.fromListN n

-- | /O(n + m*log n)/ Subtract the subtrahend of size @m@ from the
-- minuend of size @n@. It should be possible to improve the improve
-- the performance of this to /O(n + m)/. Anyone interested in doing
-- this should open a PR.
difference :: (Ord a, Enum a)
  => Set a -- ^ minuend
  -> Set a -- ^ subtrahend
  -> Set a
difference (Set x) (Set y) = Set (I.difference x y)

-- | The intersection of two diet sets.
intersection :: (Ord a, Enum a)
  => Set a -- ^ minuend
  -> Set a -- ^ subtrahend
  -> Set a
intersection (Set x) (Set y) = Set (I.intersection x y)

-- | The negation of a diet set. The resulting set contains
-- all elements that were not contained by the argument set,
-- and it only contains these elements.
negate :: (Ord a, Enum a, Bounded a)
  => Set a
  -> Set a
negate (Set x) = Set (I.negate x)

foldr :: (a -> a -> b -> b) -> b -> Set a -> b
foldr f z (Set arr) = I.foldr f z arr

-- | /O(n)/ The subset where all elements are greater than
-- or equal to the given value. 
aboveInclusive :: (Ord a)
  => a -- ^ inclusive lower bound
  -> Set a
  -> Set a
aboveInclusive x (Set s) = Set (I.aboveInclusive x s)

-- | /O(n)/ The subset where all elements are less than
-- or equal to the given value. 
belowInclusive :: (Ord a)
  => a -- ^ inclusive upper bound
  -> Set a
  -> Set a
belowInclusive x (Set s) = Set (I.belowInclusive x s)

-- | /O(n)/ The subset where all elements are greater than
-- or equal to the lower bound and less than or equal to
-- the upper bound.
betweenInclusive :: (Ord a)
  => a -- ^ inclusive lower bound
  -> a -- ^ inclusive upper bound
  -> Set a
  -> Set a
betweenInclusive x y (Set s) = Set (I.betweenInclusive x y s)

