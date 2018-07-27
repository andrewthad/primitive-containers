{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}
module Data.Set.Unboxed
  ( Set
  , empty
  , singleton
  , member
  , size
  , difference
  , (\\)
  , intersection
    -- * List Conversion
  , toList
  , fromList
    -- * Folds
  , foldr
  , foldMap
  , foldl'
  , foldr'
  , foldMap'
    -- * Traversals
  , itraverse_
  ) where

import Prelude hiding (foldr,foldMap)
import Data.Primitive.Types (Prim)
import Data.Primitive.UnliftedArray (PrimUnlifted(..))
import Data.Primitive.PrimArray (PrimArray)
import Data.Semigroup (Semigroup)
import Data.Hashable (Hashable)
import qualified Data.Foldable as F
import qualified Data.Hashable as H
import qualified Data.Semigroup as SG
import qualified GHC.Exts as E
import qualified Data.Set.Internal as I

-- | A set of elements.
newtype Set a = Set (I.Set PrimArray a)

instance PrimUnlifted (Set a) where
  toArrayArray# (Set x) = toArrayArray# x
  fromArrayArray# y = Set (fromArrayArray# y)

instance (Prim a, Ord a) => Semigroup (Set a) where
  Set x <> Set y = Set (I.append x y)
  stimes = SG.stimesIdempotentMonoid
  sconcat xs = Set (I.concat (E.coerce (F.toList xs)))

instance (Prim a, Ord a) => Monoid (Set a) where
  mempty = Set I.empty
  mappend = (SG.<>)
  mconcat xs = Set (I.concat (E.coerce xs))

instance (Prim a, Eq a) => Eq (Set a) where
  Set x == Set y = I.equals x y

instance (Prim a, Ord a) => Ord (Set a) where
  compare (Set x) (Set y) = I.compare x y

instance (Hashable a, Prim a) => Hashable (Set a) where
  hashWithSalt s (Set arr) = I.liftHashWithSalt H.hashWithSalt s arr

-- | The functions that convert a list to a 'Set' are asymptotically
-- better that using @'foldMap' 'singleton'@, with a cost of /O(n*log n)/
-- rather than /O(n^2)/. If the input list is sorted, even if duplicate
-- elements are present, the algorithm further improves to /O(n)/. The
-- fastest option available is calling 'fromListN' on a presorted list
-- and passing the correct size size of the resulting 'Set'. However, even
-- if an incorrect size is given to this function,
-- it will still correctly convert the list into a 'Set'.
instance (Prim a, Ord a) => E.IsList (Set a) where
  type Item (Set a) = a
  fromListN n = Set . I.fromListN n
  fromList = Set . I.fromList
  toList = toList

instance (Prim a, Show a) => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

-- | The empty set.
empty :: Set a
empty = Set I.empty

-- | The difference of two sets.
difference :: (Ord a, Prim a) => Set a -> Set a -> Set a
difference (Set x) (Set y) = Set (I.difference x y)

-- | Infix operator for 'difference'.
(\\) :: (Ord a, Prim a) => Set a -> Set a -> Set a
(\\) (Set x) (Set y) = Set (I.difference x y)

-- | The intersection of two sets.
intersection :: (Ord a, Prim a) => Set a -> Set a -> Set a
intersection (Set x) (Set y) = Set (I.intersection x y)

-- | Test whether or not an element is present in a set.
member :: (Prim a, Ord a) => a -> Set a -> Bool
member a (Set s) = I.member a s

-- | Construct a set with a single element.
singleton :: Prim a => a -> Set a
singleton = Set . I.singleton

-- | Convert a set to a list. The elements are given in ascending order.
toList :: Prim a => Set a -> [a]
toList (Set s) = I.toList s

-- | Convert a list to a set.
fromList :: (Ord a, Prim a) => [a] -> Set a
fromList xs = Set (I.fromList xs)

-- | The number of elements in the set.
size :: Prim a => Set a -> Int
size (Set s) = I.size s

-- | Right fold over the elements in the set. This is lazy in the accumulator.
foldr :: Prim a
  => (a -> b -> b)
  -> b
  -> Set a
  -> b
foldr f b0 (Set s) = I.foldr f b0 s

-- | Strict left fold over the elements in the set.
foldl' :: Prim a
  => (b -> a -> b)
  -> b
  -> Set a
  -> b
foldl' f b0 (Set s) = I.foldl' f b0 s

-- | Strict right fold over the elements in the set.
foldr' :: Prim a
  => (a -> b -> b)
  -> b
  -> Set a
  -> b
foldr' f b0 (Set s) = I.foldr' f b0 s

-- | Strict monoidal fold over the elements in the set.
foldMap' :: (Monoid m, Prim a)
  => (a -> m)
  -> Set a
  -> m
foldMap' f (Set arr) = I.foldMap' f arr

-- | Lazy monoidal fold over the elements in the set.
foldMap :: (Monoid m, Prim a)
  => (a -> m)
  -> Set a
  -> m
foldMap f (Set arr) = I.foldMap f arr

-- | Traverse a set with the indices, discarding the result.
itraverse_ :: (Applicative m, Prim a)
  => (Int -> a -> m b)
  -> Set a
  -> m ()
itraverse_ f (Set arr) = I.itraverse_ f arr
{-# INLINEABLE itraverse_ #-}

