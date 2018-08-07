{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

module Data.Dependent.Map.Unboxed.Lifted
  ( Map
  , empty
  , singleton
  , lookup
  , foldrWithKey
  , foldlWithKeyM'
  , foldMapWithKey
  , toList
  , fromList
  , unsafeFreezeZip
  ) where

import Prelude hiding (lookup)

import Control.Monad.ST (ST)
import Data.Aeson (FromJSON,ToJSON)
import Data.Dependent.Map.Class (Universally,ApplyUniversally)
import Data.Exists (EqForallPoly,EqForeach,OrdForeach)
import Data.Exists (OrdForallPoly,DependentPair,ShowForall,ShowForeach,ToSing)
import Data.Exists (ToJSONKeyForall,FromJSONKeyExists,ToJSONForeach,SemigroupForeach)
import Data.Exists (FromJSONForeach)
import Data.Primitive (Array,PrimArray,Prim,MutablePrimArray,MutableArray)
import Data.Semigroup (Semigroup)
import GHC.Exts (IsList,Any)

import qualified Data.Aeson as AE
import qualified Data.Semigroup as SG
import qualified Data.Dependent.Map.Internal as I
import qualified GHC.Exts

newtype Map k v = Map (I.Map PrimArray Array k v)

empty :: Map k v
empty = Map I.empty

singleton :: Universally k Prim => k a -> v a -> Map k v
singleton f v = Map (I.singleton f v)

lookup :: (Universally k Prim, ApplyUniversally k Prim, OrdForallPoly k) => k a -> Map k v -> Maybe (v a)
lookup k (Map x) = I.lookup k x

fromList :: (Universally k Prim, ApplyUniversally k Prim, OrdForallPoly k) => [DependentPair k v] -> Map k v
fromList xs = Map (I.fromList xs)

fromListN :: (Universally k Prim, ApplyUniversally k Prim, OrdForallPoly k) => Int -> [DependentPair k v] -> Map k v
fromListN n xs = Map (I.fromListN n xs)

toList :: Universally k Prim => Map k v -> [DependentPair k v]
toList (Map x) = I.toList x

foldrWithKey :: 
     Universally k Prim
  => (forall a. k a -> v a -> b -> b)
  -> b
  -> Map k v
  -> b
foldrWithKey f b (Map m) = I.foldrWithKey f b m

foldlWithKeyM' :: 
     (Universally k Prim, Monad m)
  => (forall a. b -> k a -> v a -> m b)
  -> b
  -> Map k v
  -> m b
foldlWithKeyM' f b (Map m) = I.foldlWithKeyM' f b m

foldMapWithKey :: 
     (Universally k Prim, Monoid m)
  => (forall a. k a -> v a -> m)
  -> Map k v
  -> m
foldMapWithKey f (Map m) = I.foldMapWithKey f m

-- | This function is really unsafe. The user needs to use unsafeCoerce to even use it.
unsafeFreezeZip :: 
     (Universally k Prim, OrdForallPoly k)
  => MutablePrimArray s (k Any)
  -> MutableArray s (v Any)
  -> ST s (Map k v)
{-# INLINABLE unsafeFreezeZip #-}
unsafeFreezeZip keys0 vals0 =
  fmap Map (I.unsafeFreezeZip keys0 vals0)

instance (Universally k Prim, ApplyUniversally k Prim, OrdForallPoly k) => IsList (Map k v) where
  type Item (Map k v) = DependentPair k v
  fromListN = fromListN
  fromList = fromList
  toList = toList
  
instance (Universally k Prim, ApplyUniversally k Prim, ShowForall k, ToSing k, ShowForeach v) => Show (Map k v) where
  showsPrec p (Map s) = I.showsPrec p s

instance (Universally k Prim, ApplyUniversally k Prim, EqForallPoly k, ToSing k, EqForeach v) => Eq (Map k v) where
  Map x == Map y = I.equals x y

instance (Universally k Prim, ApplyUniversally k Prim, OrdForallPoly k, ToSing k, OrdForeach v) => Ord (Map k v) where
  compare (Map x) (Map y) = I.compare x y

instance (Universally k Prim, ToSing k, ToJSONKeyForall k, ToJSONForeach v) => ToJSON (Map k v) where
  toJSON (Map m) = I.toJSON m

instance (Universally k Prim, ApplyUniversally k Prim, ToSing k, FromJSONKeyExists k, FromJSONForeach v, OrdForallPoly k) => FromJSON (Map k v) where
  parseJSON v = fmap Map (I.parseJSON v)

instance (Universally k Prim, ToSing k, OrdForallPoly k, SemigroupForeach v) => Semigroup (Map k v) where
  Map x <> Map y = Map (I.append x y)

instance (Universally k Prim, ToSing k, OrdForallPoly k, SemigroupForeach v) => Monoid (Map k v) where
  mempty = Map I.empty
  mappend = (SG.<>)

