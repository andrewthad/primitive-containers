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
  ) where

import Data.Primitive.UnliftedArray (PrimUnlifted(..))
import qualified Data.Foldable as F
import qualified Data.Semigroup as SG
import qualified GHC.Exts as E
import qualified Internal.Set.Lifted as I

newtype Set a = Set (I.Set a)

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

instance Ord a => Ord (Set a) where
  compare (Set x) (Set y) = I.compare x y

instance Ord a => E.IsList (Set a) where
  type Item (Set a) = a
  fromListN n = Set . I.fromListN n
  fromList = Set . I.fromList
  toList (Set s) = I.toList s

instance Show a => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

member :: Ord a => a -> Set a -> Bool
member a (Set s) = I.member a s

singleton :: a -> Set a
singleton = Set . I.singleton


