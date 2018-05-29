{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}
module Data.Set.Lifted
  ( Set
  , singleton
  , member
  , size
  , difference
  , (\\)
    -- * List Conversion
  , toList
  , fromList
    -- * Folds
  , foldr
  , foldl'
  , foldr'
  , foldMap'
  ) where

import Prelude hiding (foldr)
import Data.Primitive.UnliftedArray (PrimUnlifted(..))
import Data.Semigroup (Semigroup)
import Data.Primitive (Array)
import Data.Functor.Classes (Eq1(liftEq),Show1(liftShowsPrec))
import Text.Show (showListWith)
import qualified Data.Foldable as F
import qualified Data.Semigroup as SG
import qualified GHC.Exts as E
import qualified Data.Set.Internal as I

newtype Set a = Set (I.Set Array a)

instance F.Foldable Set where
  foldr = foldr
  foldl' = foldl'
  foldr' = foldr'

instance PrimUnlifted (Set a) where
  toArrayArray# (Set x) = toArrayArray# x
  fromArrayArray# y = Set (fromArrayArray# y)

instance Ord a => Semigroup (Set a) where
  Set x <> Set y = Set (I.append x y)
  stimes = SG.stimesIdempotentMonoid
  sconcat xs = Set (I.concat (E.coerce (F.toList xs)))

instance Ord a => Monoid (Set a) where
  mempty = Set I.empty
  mappend = (SG.<>)
  mconcat xs = Set (I.concat (E.coerce xs))

instance Eq a => Eq (Set a) where
  Set x == Set y = I.equals x y

instance Eq1 Set where
  liftEq f a b = liftEq f (toList a) (toList b)

instance Ord a => Ord (Set a) where
  compare (Set x) (Set y) = I.compare x y

instance Ord a => E.IsList (Set a) where
  type Item (Set a) = a
  fromListN n = Set . I.fromListN n
  fromList = Set . I.fromList
  toList = toList

instance Show a => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

instance Show1 Set where
  liftShowsPrec f _ p s = showParen (p > 10) $
   showString "fromList " . showListWith (f 0) (toList s)

-- | The difference of two sets.
difference :: Ord a => Set a -> Set a -> Set a
difference (Set x) (Set y) = Set (I.difference x y)

-- | Infix operator for 'difference'.
(\\) :: Ord a => Set a -> Set a -> Set a
(\\) (Set x) (Set y) = Set (I.difference x y)

-- | Test whether or not an element is present in a set.
member :: Ord a => a -> Set a -> Bool
member a (Set s) = I.member a s

-- | Construct a set with a single element.
singleton :: a -> Set a
singleton = Set . I.singleton

-- | Convert a set to a list. The elements are given in ascending order.
toList :: Set a -> [a]
toList (Set s) = I.toList s

-- | Convert a list to a set.
fromList :: Ord a => [a] -> Set a
fromList = Set . I.fromList

-- | The number of elements in the set.
size :: Set a -> Int
size (Set s) = I.size s

-- | Right fold over the elements in the set. This is lazy in the accumulator.
foldr :: 
     (a -> b -> b)
  -> b
  -> Set a
  -> b
foldr f b0 (Set s) = I.foldr f b0 s

-- | Strict left fold over the elements in the set.
foldl' :: 
     (b -> a -> b)
  -> b
  -> Set a
  -> b
foldl' f b0 (Set s) = I.foldl' f b0 s

-- | Strict right fold over the elements in the set.
foldr' :: 
     (a -> b -> b)
  -> b
  -> Set a
  -> b
foldr' f b0 (Set s) = I.foldr' f b0 s

-- | Strict monoidal fold over the elements in the set.
foldMap' :: Monoid m
  => (a -> m)
  -> Set a
  -> m
foldMap' f (Set arr) = I.foldMap' f arr

