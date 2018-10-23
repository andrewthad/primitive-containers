{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}
module Data.Diet.Map.Strict.Unboxed.Lifted
  ( Map
  , empty
  , singleton
  , lookup
  , mapBijection
  , fromSet
    -- * List Conversion
  , fromList
  , fromListAppend
  , fromListN
  , fromListAppendN
  ) where

import Prelude hiding (lookup,map)

import Data.Diet.Set.Unboxed (Set(..))
import Data.Functor.Classes (Show2(..))
import Data.Primitive.Array (Array)
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.Types (Prim)
import Data.Semigroup (Semigroup)
import qualified GHC.Exts as E
import qualified Data.Semigroup as SG
import qualified Data.Diet.Map.Strict.Internal as I

newtype Map k v = Map (I.Map PrimArray Array k v)

-- | The empty diet map.
empty :: Map k v
empty = Map I.empty

-- | /O(1)/ Create a diet map with a single element.
singleton :: (Prim k,Ord k)
  => k -- ^ inclusive lower bound
  -> k -- ^ inclusive upper bound
  -> v -- ^ value
  -> Map k v
singleton lo hi v = Map (I.singleton lo hi v)

-- | /O(log n)/ Lookup the value at a key in the map.
lookup :: (Prim k, Ord k) => k -> Map k v -> Maybe v
lookup a (Map s) = I.lookup a s

instance (Prim k, Show k, Show v) => Show (Map k v) where
  showsPrec p (Map m) = I.showsPrec p m

instance (Prim k, Eq k, Eq v) => Eq (Map k v) where
  Map x == Map y = I.equals x y

instance (Prim k, Ord k, Enum k, Semigroup v, Eq v) => Semigroup (Map k v) where
  Map x <> Map y = Map (I.append x y)

instance (Prim k, Ord k, Enum k, Semigroup v, Eq v) => Monoid (Map k v) where
  mempty = Map I.empty
  mappend = (SG.<>)
  mconcat = Map . I.concat . E.coerce

instance (Prim k, Ord k, Enum k, Eq v) => E.IsList (Map k v) where
  type Item (Map k v) = (k,k,v)
  fromListN n = Map . I.fromListN n
  fromList = Map . I.fromList
  toList (Map s) = I.toList s

fromList :: (Ord k, Enum k, Prim k, Eq v) => [(k,k,v)] -> Map k v
fromList = Map . I.fromList

fromListN :: (Ord k, Enum k, Prim k, Eq v)
  => Int -- ^ expected size of resulting 'Map'
  -> [(k,k,v)] -- ^ key-value pairs
  -> Map k v
fromListN n = Map . I.fromListN n

fromListAppend :: (Ord k, Enum k, Prim k, Semigroup v, Eq v) => [(k,k,v)] -> Map k v
fromListAppend = Map . I.fromListAppend

fromListAppendN :: (Ord k, Enum k, Prim k, Semigroup v, Eq v)
  => Int -- ^ expected size of resulting 'Map'
  -> [(k,k,v)] -- ^ key-value pairs
  -> Map k v
fromListAppendN n = Map . I.fromListAppendN n

-- | Map an equality morphism over the values in a diet map. An bijection
-- @f@ must satisfy the law:
--
-- > ∀ x y. x == y ↔ f x == f y
--
-- Since this does not actually use the 'Eq' constraint on the new value
-- type, it is lazy in the values.
mapBijection :: (Prim k, Ord k)
  => (v -> w) -- ^ bijection
  -> Map k v
  -> Map k w
mapBijection f (Map m) = Map (I.map f m)

-- | Convert a diet set to a diet map, constructing each value
-- from the low and high key in its corresponding range.
fromSet :: Prim k
  => (k -> k -> v)
  -> Set k
  -> Map k v
fromSet f (Set s) = Map (I.fromSet f s)
