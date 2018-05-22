{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}
module Data.Diet.Set.Unboxed
  ( Set
  , singleton
  , member
    -- * List Conversion
  , toList
  , fromList
  , fromListN
  ) where

import Prelude hiding (lookup,map)

import Data.Semigroup (Semigroup)
import Data.Functor.Classes (Show2(..))
import Data.Primitive.Types (Prim)
import Data.Primitive.PrimArray (PrimArray)
import qualified GHC.Exts as E
import qualified Data.Semigroup as SG
import qualified Data.Diet.Set.Internal as I

newtype Set a = Set (I.Set PrimArray a)

-- | /O(1)/ Create a diet set with a single element.
singleton :: (Ord a, Prim a)
  => a -- ^ inclusive lower bound
  -> a -- ^ inclusive upper bound
  -> Set a
singleton lo hi = Set (I.singleton lo hi)

-- | /O(log n)/ Lookup the value at a key in the map.
member :: (Ord a, Prim a) => a -> Set a -> Bool
member a (Set s) = I.member a s

instance (Show a, Prim a) => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

instance (Eq a, Prim a) => Eq (Set a) where
  Set x == Set y = I.equals x y

instance (Ord a, Prim a) => Ord (Set a) where
  compare (Set xs) (Set ys) = compare (I.toList xs) (I.toList ys)

instance (Ord a, Enum a, Prim a) => Semigroup (Set a) where
  Set x <> Set y = Set (I.append x y)

instance (Ord a, Enum a, Prim a) => Monoid (Set a) where
  mempty = Set I.empty
  mappend = (SG.<>)
  mconcat = Set . I.concat . E.coerce

instance (Ord a, Enum a, Prim a) => E.IsList (Set a) where
  type Item (Set a) = (a,a)
  fromListN n = Set . I.fromListN n
  fromList = Set . I.fromList
  toList (Set s) = I.toList s

toList :: Prim a => Set a -> [(a,a)]
toList (Set x) = I.toList x

fromList :: (Ord a, Enum a, Prim a) => [(a,a)] -> Set a
fromList = Set . I.fromList

fromListN :: (Ord a, Enum a, Prim a)
  => Int -- ^ expected size of resulting diet 'Set'
  -> [(a,a)] -- ^ key-value pairs
  -> Set a
fromListN n = Set . I.fromListN n

