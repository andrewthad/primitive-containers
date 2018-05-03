{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Set.Unboxed
  ( Set
  , singleton
  , member
  ) where

import Data.Primitive.Types (Prim)
import qualified GHC.Exts as E
import qualified Data.Semigroup as SG
import qualified Internal.Set.Unboxed as I

newtype Set a = Set (I.Set a)

instance (Prim a, Ord a) => Semigroup (Set a) where
  Set x <> Set y = Set (I.append x y)

instance (Prim a, Ord a) => Monoid (Set a) where
  mempty = Set I.empty
  mappend = (SG.<>)

instance (Prim a, Eq a) => Eq (Set a) where
  Set x == Set y = I.equals x y

instance (Prim a, Ord a) => Ord (Set a) where
  compare (Set x) (Set y) = I.compare x y

instance (Prim a, Ord a) => E.IsList (Set a) where
  type Item (Set a) = a
  fromListN _ = foldMap singleton
  fromList = foldMap singleton
  toList (Set s) = I.toList s

instance (Prim a, Show a) => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

member :: (Prim a, Ord a) => a -> Set a -> Bool
member a (Set s) = I.member a s

singleton :: Prim a => a -> Set a
singleton = Set . I.singleton

