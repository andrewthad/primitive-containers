{-# language GeneralizedNewtypeDeriving #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}

module Data.Dependent.Map.Lifted.Lifted
  ( Map
  , singleton
  , lookup
  , toList
  , fromList
  ) where

import Prelude hiding (lookup)

import Data.Primitive (Array)
import Data.Semigroup (Semigroup)
import Data.Exists (OrdForallPoly,DependentPair,ShowForall,ShowForeach,ToSing)
import Data.Exists (EqForallPoly,EqForeach,OrdForeach,SemigroupForeach)
import GHC.Exts (IsList)

import qualified Data.Dependent.Map.Internal as I
import qualified Data.Semigroup as SG
import qualified GHC.Exts

newtype Map k v = Map (I.Map Array Array k v)

singleton :: k a -> v a -> Map k v
singleton f v = Map (I.singleton f v)

lookup :: OrdForallPoly k => k a -> Map k v -> Maybe (v a)
lookup k (Map x) = I.lookup k x

fromList :: OrdForallPoly k => [DependentPair k v] -> Map k v
fromList xs = Map (I.fromList xs)

fromListN :: OrdForallPoly k => Int -> [DependentPair k v] -> Map k v
fromListN n xs = Map (I.fromListN n xs)

toList :: Map k v -> [DependentPair k v]
toList (Map x) = I.toList x

instance OrdForallPoly k => IsList (Map k v) where
  type Item (Map k v) = DependentPair k v
  fromListN = fromListN
  fromList = fromList
  toList = toList
  
instance (ShowForall k, ToSing k, ShowForeach v) => Show (Map k v) where
  showsPrec p (Map s) = I.showsPrec p s

instance (EqForallPoly k, ToSing k, EqForeach v) => Eq (Map k v) where
  Map x == Map y = I.equals x y

instance (OrdForallPoly k, ToSing k, OrdForeach v) => Ord (Map k v) where
  compare (Map x) (Map y) = I.compare x y

instance (ToSing k, OrdForallPoly k, SemigroupForeach v) => Semigroup (Map k v) where
  Map x <> Map y = Map (I.append x y)

instance (ToSing k, OrdForallPoly k, SemigroupForeach v) => Monoid (Map k v) where
  mempty = Map I.empty
  mappend = (SG.<>)

