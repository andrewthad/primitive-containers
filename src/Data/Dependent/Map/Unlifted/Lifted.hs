{-# language FlexibleContexts #-}

module Data.Dependent.Map.Unlifted.Lifted
  ( Map
  , singleton
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.Primitive (Array,UnliftedArray,PrimUnlifted)
import Data.Dependent.Map.Class
import Data.Exists (OrdForallPoly)
import qualified Data.Dependent.Map.Internal as I

newtype Map k v = Map (I.Map UnliftedArray Array k v)

singleton :: ApplyUniversally k PrimUnlifted => k a -> v a -> Map k v
singleton f v = Map (I.singleton f v)

lookup :: (OrdForallPoly k, ApplyUniversally k PrimUnlifted) => k a -> Map k v -> Maybe (v a)
lookup k (Map x) = I.lookup k x

