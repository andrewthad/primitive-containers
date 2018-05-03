{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Set.Lifted
  ( Set
  , singleton
  , member
  ) where

import qualified GHC.Exts as E
import qualified Data.Semigroup as SG
import qualified Internal.Set.Lifted as I

newtype Set a = Set (I.Set a)

instance Ord a => Semigroup (Set a) where
  Set x <> Set y = Set (I.append x y)

instance Ord a => Monoid (Set a) where
  mempty = Set I.empty
  mappend = (SG.<>)

instance Eq a => Eq (Set a) where
  Set x == Set y = I.equals x y

instance Ord a => Ord (Set a) where
  compare (Set x) (Set y) = I.compare x y

instance Ord a => E.IsList (Set a) where
  type Item (Set a) = a
  fromListN _ = foldMap singleton
  fromList = foldMap singleton
  toList (Set s) = I.toList s

instance Show a => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

member :: Ord a => a -> Set a -> Bool
member a (Set s) = I.member a s

singleton :: a -> Set a
singleton = Set . I.singleton


