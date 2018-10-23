{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Set.Lifted.Internal
  ( Set(..)
  , toList
  , fromList
  , foldr
  , foldl'
  , foldr'
  ) where

import Prelude hiding (foldr)

import Data.Primitive.UnliftedArray (PrimUnlifted(..))
import Data.Functor.Classes (Eq1(liftEq),Show1(liftShowsPrec))
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1)
import Data.Primitive (Array)
import Data.Semigroup (Semigroup)
import Text.Show (showListWith)

import qualified Data.Foldable as F
import qualified Data.Hashable as H
import qualified Data.Hashable.Lifted as HL
import qualified Data.Semigroup as SG
import qualified Data.Set.Internal as I
import qualified GHC.Exts as E

newtype Set a = Set { getSet :: I.Set Array a }

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

instance Hashable1 Set where
  liftHashWithSalt f s (Set arr) = I.liftHashWithSalt f s arr

instance Hashable a => Hashable (Set a) where
  hashWithSalt = HL.hashWithSalt1

-- | Convert a set to a list. The elements are given in ascending order.
toList :: Set a -> [a]
toList (Set s) = I.toList s

-- | Convert a list to a set.
fromList :: Ord a => [a] -> Set a
fromList = Set . I.fromList

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
