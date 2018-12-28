{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}
module Data.Set.Unlifted
  ( S.Set
  , empty
  , singleton
  , null
  , member
  , size
  , difference
  , intersection
  , enumFromTo
    -- * Conversion
  , toArray
  , S.toList
  , S.fromList
    -- * Folds
  , foldr
  , foldMap
  , foldl'
  , foldr'
  , foldMap'
    -- * Traversals
  , traverse_
  , itraverse_
  ) where

import Prelude hiding (foldr,foldMap,null,enumFromTo)

import Data.Primitive.UnliftedArray (UnliftedArray, PrimUnlifted(..))
import Data.Semigroup (Semigroup)
import Data.Set.Unlifted.Internal (Set(..))
import qualified Data.Set.Internal as I
import qualified Data.Set.Unlifted.Internal as S

-- | Test for membership in the set.
member :: (PrimUnlifted a, Ord a) => a -> Set a -> Bool
member a (Set s) = I.member a s

-- | The empty set.
empty :: Set a
empty = Set I.empty

-- | True if the set is empty
null :: Set a -> Bool
null (Set s) = I.null s

-- | Construct a set with a single element.
singleton :: PrimUnlifted a => a -> Set a
singleton = Set . I.singleton

-- | The number of elements in the set.
size :: PrimUnlifted a => Set a -> Int
size (Set s) = I.size s

-- | The difference of two sets.
difference :: (PrimUnlifted a, Ord a) => Set a -> Set a -> Set a
difference (Set x) (Set y) = Set (I.difference x y)

-- | The intersection of two sets.
intersection :: (Ord a, PrimUnlifted a) => Set a -> Set a -> Set a
intersection (Set x) (Set y) = Set (I.intersection x y)

-- | The set that includes all elements from the lower bound to the
-- upper bound.
enumFromTo :: (Enum a, Ord a, Num a, PrimUnlifted a)
  => a -- ^ Inclusive lower bound
  -> a -- ^ Inclusive upper bound
  -> Set a
enumFromTo lo hi = Set (I.enumFromTo lo hi)

-- | /O(1)/ Convert a set to an array. The elements are given in ascending
-- order. This function is zero-cost.
toArray :: Set a -> UnliftedArray a
toArray (Set s) = I.toArray s

-- | Right fold over the elements in the set. This is lazy in the accumulator.
foldr :: PrimUnlifted a
  => (a -> b -> b)
  -> b
  -> Set a
  -> b
foldr f b0 (Set s) = I.foldr f b0 s

-- | Monoidal fold over the elements in the set. This is lazy in the accumulator.
foldMap :: (PrimUnlifted a, Monoid m)
  => (a -> m)
  -> Set a
  -> m
foldMap f (Set s) = I.foldMap f s

-- | Strict left fold over the elements in the set.
foldl' :: PrimUnlifted a
  => (b -> a -> b)
  -> b
  -> Set a
  -> b
foldl' f b0 (Set s) = I.foldl' f b0 s

-- | Strict right fold over the elements in the set.
foldr' :: PrimUnlifted a
  => (a -> b -> b)
  -> b
  -> Set a
  -> b
foldr' f b0 (Set s) = I.foldr' f b0 s

-- | Strict monoidal fold over the elements in the set.
foldMap' :: (PrimUnlifted a, Monoid m)
  => (a -> m)
  -> Set a
  -> m
foldMap' f (Set arr) = I.foldMap' f arr

-- | Traverse a set, discarding the result.
traverse_ :: (Applicative m, PrimUnlifted a)
  => (a -> m b)
  -> Set a
  -> m ()
traverse_ f (Set arr) = I.traverse_ f arr

-- | Traverse a set with the indices, discarding the result.
itraverse_ :: (Applicative m, PrimUnlifted a)
  => (Int -> a -> m b)
  -> Set a
  -> m ()
itraverse_ f (Set arr) = I.itraverse_ f arr

