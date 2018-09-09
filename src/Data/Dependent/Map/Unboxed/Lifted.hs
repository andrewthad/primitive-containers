{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

module Data.Dependent.Map.Unboxed.Lifted
  ( Map
  , empty
  , null
  , singleton
  , lookup
  , foldrWithKey
  , foldlWithKeyM'
  , foldMapWithKey
  , traverseWithKey_
  , toList
  , fromList
  , map
  , mapWithKey
  , mapMaybe
  , mapMaybeWithKey
  , size
    -- * Unsafe Functions
  , unsafeFreezeZip
  , unsafeCoerceKeys
  ) where

import Prelude hiding (lookup,null,map)

import Control.Monad.ST (ST)
import Data.Aeson (FromJSON,ToJSON)
import Data.Dependent.Map.Class (Universally,ApplyUniversally)
import Data.Exists (EqForallPoly,EqForeach,OrdForeach)
import Data.Exists (OrdForallPoly,DependentPair,ShowForall,ShowForeach,ToSing)
import Data.Exists (ToJSONKeyForall,FromJSONKeyExists,ToJSONForeach,SemigroupForeach)
import Data.Exists (FromJSONForeach)
import Data.Primitive (Array,PrimArray,Prim,MutablePrimArray,MutableArray)
import Data.Proxy (Proxy)
import Data.Semigroup (Semigroup)
import GHC.Exts (IsList,Any)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Aeson as AE
import qualified Data.Semigroup as SG
import qualified Data.Dependent.Map.Internal as I
import qualified GHC.Exts
import qualified Data.Set.Unboxed.Internal as SU
import qualified Data.Map.Internal as M

newtype Map k v = Map (I.Map PrimArray Array k v)

empty :: Map k v
empty = Map I.empty

null :: Map k v -> Bool
null (Map m) = I.null m

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

size :: Map k v -> Int
size (Map x) = I.size x

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

traverseWithKey_ :: 
     (Universally k Prim, Applicative m)
  => (forall a. k a -> v a -> m b)
  -> Map k v
  -> m ()
traverseWithKey_ f (Map m) = I.traverseWithKey_ f m

map ::
     Universally k Prim
  => (forall a. v a -> w a)
  -> Map k v
  -> Map k w
map f (Map m) = Map (I.map f m)

mapMaybe ::
     Universally k Prim
  => (forall a. v a -> Maybe (w a))
  -> Map k v
  -> Map k w
mapMaybe f (Map m) = Map (I.mapMaybe f m)

mapMaybeWithKey ::
     Universally k Prim
  => (forall a. k a -> v a -> Maybe (w a))
  -> Map k v
  -> Map k w
mapMaybeWithKey f (Map m) = Map (I.mapMaybeWithKey f m)

mapWithKey ::
     Universally k Prim
  => (forall a. k a -> v a -> w a)
  -> Map k v
  -> Map k w
mapWithKey f (Map m) = Map (I.mapWithKey f m)

-- | This function is even more unsafe than the @unsafeFreezeZip@ provided by
-- @Data.Map.Unboxed.Lifted@. The user needs to use @unsafeCoerce@ to even use this
-- function.
unsafeFreezeZip :: 
     (Universally k Prim, OrdForallPoly k)
  => MutablePrimArray s (k Any)
  -> MutableArray s (v Any)
  -> ST s (Map k v)
{-# INLINABLE unsafeFreezeZip #-}
unsafeFreezeZip keys0 vals0 =
  fmap Map (I.unsafeFreezeZip keys0 vals0)

-- | /O(1)/ This function is highly unsafe. The user is responsible for ensuring
-- that:
--
-- * Both @k'@ and @forall a. k a@ have the same runtime representation.
-- * The @Ord@ instance for @k'@ agrees with the @OrdForallPoly@ instance
--   for @k@.
unsafeCoerceKeys :: Proxy k' -> Map k v -> SU.Set k'
unsafeCoerceKeys p (Map (I.Map m)) =
  -- TODO: Technical debt. Add this function to the Internal module
  -- so that the data constructor does not have to be exported.
  unsafeCoerceSet p (SU.Set (M.keys m))

unsafeCoerceSet :: Proxy k' -> SU.Set (k Any) -> SU.Set k'
unsafeCoerceSet _ = unsafeCoerce

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

instance (ApplyUniversally k Prim, Universally k Prim, ToSing k, OrdForallPoly k, SemigroupForeach v) => Semigroup (Map k v) where
  Map x <> Map y = Map (I.append x y)

instance (ApplyUniversally k Prim, Universally k Prim, ToSing k, OrdForallPoly k, SemigroupForeach v) => Monoid (Map k v) where
  mempty = Map I.empty
  mappend = (SG.<>)

