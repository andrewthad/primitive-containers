{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Map.Subset.Lifted
  ( I.Map
  , singleton
  , lookup
  , toList
  , fromList
  ) where

import Prelude hiding (lookup)

import Data.Map.Subset.Internal (Map)
import Data.Set.Lifted.Internal (Set(..))
import Data.Bifunctor (first)
import Data.Semigroup (Semigroup)

import qualified Data.Map.Subset.Internal as I

singleton :: (Monoid v, Eq v)
  => Set k
  -> v
  -> Map k v
singleton (Set s) v = I.singleton s v

lookup :: Ord k => Set k -> Map k v -> Maybe v
lookup (Set s) m = I.lookup s m

toList :: Map k v -> [(Set k,v)]
toList = map (first Set) . I.toList

fromList :: (Ord k, Eq v, Semigroup v) => [(Set k,v)] -> Map k v
fromList = I.fromList . map (first getSet)
