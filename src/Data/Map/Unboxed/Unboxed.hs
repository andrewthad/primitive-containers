{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Map.Unboxed.Unboxed
  ( Map
  , singleton
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.Primitive.Types (Prim)
import qualified GHC.Exts as E
import qualified Data.Semigroup as SG
import qualified Internal.Map.Unboxed.Unboxed as I

newtype Map k v = Map (I.Map k v)

instance (Prim k, Ord k, Prim v, Semigroup v) => Semigroup (Map k v) where
  Map x <> Map y = Map (I.append x y)

instance (Prim k, Ord k, Prim v, Monoid v, Semigroup v) => Monoid (Map k v) where
  mempty = Map I.empty
  mappend = (SG.<>)

instance (Prim k, Eq k, Prim v, Eq v) => Eq (Map k v) where
  Map x == Map y = I.equals x y

instance (Prim k, Ord k, Prim v, Ord v) => Ord (Map k v) where
  compare (Map x) (Map y) = I.compare x y

instance (Prim k, Ord k, Prim v) => E.IsList (Map k v) where
  type Item (Map k v) = (k,v)
  fromListN n = Map . I.fromListN n
  fromList = Map . I.fromList
  toList (Map s) = I.toList s

instance (Prim k, Show k, Prim v, Show v) => Show (Map k v) where
  showsPrec p (Map s) = I.showsPrec p s

lookup :: (Prim k, Ord k, Prim v) => k -> Map k v -> Maybe v
lookup a (Map s) = I.lookup a s

singleton :: (Prim k, Prim v) => k -> v -> Map k v
singleton k v = Map (I.singleton k v)

