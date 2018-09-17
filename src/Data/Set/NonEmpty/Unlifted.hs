{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}
module Data.Set.NonEmpty.Unlifted
  ( Set
  , singleton
  , member
  , size
    -- * Conversion
  , toArray
  , toList
  , fromNonEmpty
  , fromSet
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

import Prelude hiding (foldr,foldMap,null)

import Data.Hashable (Hashable)
import Data.Primitive.UnliftedArray (PrimUnlifted(..),UnliftedArray)
import Data.Semigroup (Semigroup)
import Data.List.NonEmpty (NonEmpty)

import qualified Data.Foldable as F
import qualified Data.Hashable as H
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as SG
import qualified Data.Set.Internal as I
import qualified GHC.Exts as E
import qualified Data.Set.Unlifted as S
import qualified Data.Set.Unlifted.Internal as SI

newtype Set a = Set (I.Set UnliftedArray a)

instance PrimUnlifted (Set a) where
  toArrayArray# (Set x) = toArrayArray# x
  fromArrayArray# y = Set (fromArrayArray# y)

instance (Ord a, PrimUnlifted a) => Semigroup (Set a) where
  Set x <> Set y = Set (I.append x y)
  stimes = SG.stimesIdempotent
  sconcat xs = Set (I.concat (E.coerce (F.toList xs)))

instance (Hashable a, PrimUnlifted a) => Hashable (Set a) where
  hashWithSalt s (Set arr) = I.liftHashWithSalt H.hashWithSalt s arr

instance (PrimUnlifted a, Eq a) => Eq (Set a) where
  Set x == Set y = I.equals x y

instance (PrimUnlifted a, Ord a) => Ord (Set a) where
  compare (Set x) (Set y) = I.compare x y

instance (PrimUnlifted a, Show a) => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

-- | /O(n)/ Convert a set to a list. The elements are given in ascending order.
toList :: PrimUnlifted a => Set a -> [a]
toList (Set s) = I.toList s

-- | /O(n*log n)/ Convert a list to a set.
fromNonEmpty :: (PrimUnlifted a, Ord a) => NonEmpty a -> Set a
fromNonEmpty = Set . I.fromList . NE.toList

-- | /O(1)/ Convert a set to a non-empty set. This returns @Nothing@ if
-- the set is empty. The resulting non-empty set shares the share internal
-- represention as the argument.
fromSet :: SI.Set a -> Maybe (Set a)
fromSet s@(SI.Set x) = if S.null s
  then Nothing
  else Just (Set x)

-- | Test for membership in the set.
member :: (PrimUnlifted a, Ord a) => a -> Set a -> Bool
member a (Set s) = I.member a s

-- | Construct a set with a single element.
singleton :: PrimUnlifted a => a -> Set a
singleton = Set . I.singleton

-- | The number of elements in the set.
size :: PrimUnlifted a => Set a -> Int
size (Set s) = I.size s

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

