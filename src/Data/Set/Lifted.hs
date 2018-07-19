{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Set.Lifted
  ( Set
  , empty
  , singleton
  , member
  , size
  , difference
  , (\\)
    -- * Conversion
  , toArray
  , LI.toList
  , LI.fromList
    -- * Folds
  , LI.foldr
  , LI.foldl'
  , LI.foldr'
  , foldMap'
  , foldMap
  ) where

import Prelude hiding (foldr,foldMap)
import Data.Semigroup (Semigroup)
import Data.Set.Lifted.Internal (Set(..))
import Data.Primitive (Array)
import qualified Data.Set.Internal as I
import qualified Data.Set.Lifted.Internal as LI

-- | The difference of two sets.
difference :: Ord a => Set a -> Set a -> Set a
difference (Set x) (Set y) = Set (I.difference x y)

-- | The empty set.
empty :: Set a
empty = Set I.empty

-- | Infix operator for 'difference'.
(\\) :: Ord a => Set a -> Set a -> Set a
(\\) (Set x) (Set y) = Set (I.difference x y)

-- | Test whether or not an element is present in a set.
member :: Ord a => a -> Set a -> Bool
member a (Set s) = I.member a s

-- | Construct a set with a single element.
singleton :: a -> Set a
singleton = Set . I.singleton

-- | The number of elements in the set.
size :: Set a -> Int
size (Set s) = I.size s

-- | Strict monoidal fold over the elements in the set.
foldMap' :: Monoid m
  => (a -> m)
  -> Set a
  -> m
foldMap' f (Set arr) = I.foldMap' f arr

-- | Lazy monoidal fold over the elements in the set.
foldMap :: Monoid m
  => (a -> m)
  -> Set a
  -> m
foldMap f (Set arr) = I.foldMap f arr

-- | /O(1)/ Convert a set to an array. The elements are given in ascending
-- order. This function is zero-cost.
toArray :: Set a -> Array a
toArray (Set s) = I.toArray s

