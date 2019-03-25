{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Map.Interval.DBTSUU
  ( Map
  , pure
  , singleton
  , lookup
  , fromList
  , unionWith
    -- * Mapping
  , map
  , mapBijection
    -- * Traversals
  , traverseBijectionP
  , traverseBijection
    -- * Folds
  , foldl'
  , foldlM'
  , foldMap
  , foldrWithKey
  , foldlWithKeyM'
  , traverse_
    -- * Properties
  , size
    -- * Conversion
  , elems
  , toList
  , fromLiftedLifted
  ) where

import Prelude hiding (lookup,map,pure,foldMap)

import Data.Semigroup (Semigroup)
import Data.Primitive.Array (Array)
import Data.Primitive (PrimArray)
import Data.Primitive.Types (Prim)
import Control.Monad.Primitive (PrimMonad)
import qualified Data.Semigroup as SG
import qualified Data.Map.Interval.DBTS.Internal as I
import qualified Data.Map.Interval.DBTSLL as DBTSLL
import qualified GHC.Exts as E

-- | A total interval map from keys @k@ to values @v@. The key type must be discrete
--   and bounded. This map is strict in the values. The key type and the value type
--   must both have 'Prim' instances.
newtype Map k v = Map (I.Map PrimArray PrimArray k v)

instance (Prim k, Prim v, Eq k, Eq v) => Eq (Map k v) where
  Map x == Map y = I.equals x y

instance (Prim k, Prim v, Ord k, Semigroup v, Eq v) => Semigroup (Map k v) where
  Map x <> Map y = Map (I.union x y)

-- The redundant constraint is needed for GHC < 8.4
instance (Prim k, Ord k, Bounded k, Prim v, Semigroup v, Monoid v, Eq v) => Monoid (Map k v) where
  mappend = (SG.<>) 
  mempty = Map I.empty
  mconcat = Map . I.concat . E.coerce

instance (Prim k, Bounded k, Enum k, Show k, Prim v, Show v) => Show (Map k v) where
  showsPrec p (Map m) = I.showsPrec p m

instance (Prim k, Bounded k, Enum k, Ord k, Prim v, Eq v, Monoid v) => E.IsList (Map k v) where
  type Item (Map k v) = (k,k,v)
  fromList xs = Map (I.fromList mempty xs)
  toList (Map m) = I.toList m

pure :: (Prim k, Bounded k, Prim v) => v -> Map k v
pure = Map . I.pure 

singleton :: (Prim k, Bounded k, Enum k, Ord k, Prim v, Eq v)
  => v -- ^ value outside of the interval
  -> k -- ^ lower bound
  -> k -- ^ upper bound
  -> v -- ^ value inside the interval
  -> Map k v
singleton def lo hi v = Map (I.singleton def lo hi v)

-- | /O(log n)/ Lookup a key. The value corresponding to the range
--   that contains this key will be returned.
lookup :: (Ord k, Prim k, Prim v) => k -> Map k v -> v
lookup k (Map m) = I.lookup k m

-- | Create an interval map from a list of range-value triples. The first
--   argument is a default value used everywhere outside of the given
--   ranges. In the case of overlapping ranges, the leftmost value is
--   used.
fromList :: (Prim k, Bounded k, Ord k, Enum k, Prim v, Eq v)
  => v -- ^ value outside of the ranges
  -> [(k,k,v)] -- ^ low-high inclusive ranges with their corresponding values
  -> Map k v
fromList def xs = Map (I.fromList def xs)

-- | This only provides a correct result when the effectful mapping
--   is a bijection.
traverseBijectionP :: (PrimMonad m, Prim v, Prim w)
  => (v -> m w) -> Map k v -> m (Map k w)
traverseBijectionP f (Map m) = fmap Map (I.traverseP f m)

-- | This only provides a correct result when the effectful mapping
--   is a bijection.
traverseBijection :: (Applicative m, Prim v, Prim w)
  => (v -> m w) -> Map k v -> m (Map k w)
traverseBijection f (Map m) = fmap Map (I.traverse f m)

traverse_ :: (Applicative m, Prim v) => (v -> m w) -> Map k v -> m ()
traverse_ f (Map m) = I.traverse_ f m

mapBijection :: (Prim v, Prim w) => (v -> w) -> Map k v -> Map k w
mapBijection f (Map m) = Map (I.mapBijection f m)

map :: (Prim k, Prim v, Prim w, Eq w) => (v -> w) -> Map k v -> Map k w
map f (Map m) = Map (I.map f m)

foldl' :: (Prim k, Prim v)
  => (b -> v -> b)
  -> b
  -> Map k v
  -> b
foldl' f b0 (Map m) = I.foldl' f b0 m

foldlM' :: (Monad m, Prim k, Prim v)
  => (b -> v -> m b)
  -> b
  -> Map k v
  -> m b
foldlM' f b0 (Map m) = I.foldlM' f b0 m

foldMap :: (Monoid m, Prim k, Prim v)
  => (v -> m)
  -> Map k v
  -> m
foldMap f (Map m) = I.foldMap f m

unionWith :: (Ord k, Eq c, Prim k, Prim a, Prim b, Prim c)
  => (a -> b -> c)
  -> Map k a
  -> Map k b
  -> Map k c
unionWith f (Map a) (Map b) = Map (I.unionWith f a b)

foldrWithKey :: (Bounded k, Enum k, Prim k, Prim v)
  => (k -> k -> v -> b -> b)
  -> b
  -> Map k v
  -> b
foldrWithKey f z (Map m) = I.foldrWithKey f z m

foldlWithKeyM' :: (Bounded k, Enum k, Monad m, Prim k, Prim v)
  => (b -> k -> k -> v -> m b)
  -> b
  -> Map k v
  -> m b
foldlWithKeyM' f z (Map m) = I.foldlWithKeyM' f z m

-- | The number of values in the interval map. Also the number of
--   contiguous key ranges in the map.
size :: Prim v => Map k v -> Int
size (Map m) = I.size m

elems :: Map k v -> PrimArray v
elems (Map m) = I.elems m

toList :: (Bounded k, Enum k, Prim k, Prim v) => Map k v -> [(k,k,v)]
toList (Map m) = I.toList m

fromLiftedLifted :: (Prim k, Prim v) => DBTSLL.Map k v -> Map k v
fromLiftedLifted (DBTSLL.Map m) = Map (I.convertKeysValues m)
