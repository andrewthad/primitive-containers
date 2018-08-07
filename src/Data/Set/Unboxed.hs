{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}
module Data.Set.Unboxed
  ( S.Set
  , empty
  , singleton
  , null
  , member
  , size
  , difference
  , (\\)
  , intersection
    -- * List Conversion
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

import Prelude hiding (foldr,foldMap,null)
import Data.Hashable (Hashable)
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.Types (Prim)
import Data.Primitive.UnliftedArray (PrimUnlifted(..))
import Data.Semigroup (Semigroup)
import Data.Set.Unboxed.Internal (Set(..))
import qualified Data.Foldable as F
import qualified Data.Hashable as H
import qualified Data.Semigroup as SG
import qualified GHC.Exts as E
import qualified Data.Set.Internal as I
import qualified Data.Set.Unboxed.Internal as S

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

-- | /O(1)/ Is the set empty?
null :: Set a -> Bool
null (Set s) = I.null s

-- | Construct a set with a single element.
singleton :: Prim a => a -> Set a
singleton = Set . I.singleton

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

-- | Traverse a set, discarding the result.
traverse_ :: (Applicative m, Prim a)
  => (a -> m b)
  -> Set a
  -> m ()
traverse_ f (Set arr) = I.traverse_ f arr

-- | Traverse a set with the indices, discarding the result.
itraverse_ :: (Applicative m, Prim a)
  => (Int -> a -> m b)
  -> Set a
  -> m ()
itraverse_ f (Set arr) = I.itraverse_ f arr
{-# INLINEABLE itraverse_ #-}

