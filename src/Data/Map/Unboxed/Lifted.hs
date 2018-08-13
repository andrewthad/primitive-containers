{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}
module Data.Map.Unboxed.Lifted
  ( Map
  , empty
  , singleton
  , lookup
  , size
  , map
  , mapMaybe
  , mapMaybeWithKey
  , keys
  , intersectionWith
  , restrict
    -- * Folds
  , foldrWithKey
  , foldlWithKey'
  , foldrWithKey'
  , foldMapWithKey
  , foldMapWithKey'
    -- * Monadic Folds
  , foldlWithKeyM'
  , foldrWithKeyM'
  , foldlMapWithKeyM'
  , foldrMapWithKeyM'
    -- * List Conversion
  , toList
  , fromList
  , fromListAppend
  , fromListN
  , fromListAppendN
  , elems
    -- * Array Conversion
  , unsafeFreezeZip
  ) where

import Prelude hiding (lookup,map)

import Control.Monad.ST (ST)
import Data.Semigroup (Semigroup)
import Data.Primitive.Types (Prim)
import Data.Primitive (PrimArray,Array,MutablePrimArray,MutableArray)
import Data.Set.Unboxed.Internal (Set(..))
import qualified GHC.Exts as E
import qualified Data.Semigroup as SG
import qualified Data.Map.Internal as I

-- | A map from keys @k@ to values @v@. The key type must have a
--   'Prim' instance and the value type is unconstrained.
newtype Map k v = Map (I.Map PrimArray Array k v)

-- | This fails the functor laws since fmap is strict.
instance Prim k => Functor (Map k) where
  fmap = map

instance (Prim k, Ord k, Semigroup v) => Semigroup (Map k v) where
  Map x <> Map y = Map (I.append x y)

instance (Prim k, Ord k, Semigroup v) => Monoid (Map k v) where
  mempty = Map I.empty
  mappend = (SG.<>)
  mconcat = Map . I.concat . E.coerce

instance (Prim k, Eq k, Eq v) => Eq (Map k v) where
  Map x == Map y = I.equals x y

instance (Prim k, Ord k, Ord v) => Ord (Map k v) where
  compare (Map x) (Map y) = I.compare x y

instance (Prim k, Ord k) => E.IsList (Map k v) where
  type Item (Map k v) = (k,v)
  fromListN n = Map . I.fromListN n
  fromList = Map . I.fromList
  toList (Map s) = I.toList s

instance (Prim k, Show k, Show v) => Show (Map k v) where
  showsPrec p (Map s) = I.showsPrec p s

-- | /O(log n)/ Lookup the value at a key in the map.
lookup :: (Prim k, Ord k) => k -> Map k v -> Maybe v
lookup a (Map s) = I.lookup a s

-- | The empty map.
empty :: Map k v
empty = Map I.empty

-- | /O(1)/ Create a map with a single element.
singleton :: (Prim k) => k -> v -> Map k v
singleton k v = Map (I.singleton k v)

-- | /O(n)/ A list of key-value pairs in ascending order.
toList :: (Prim k, Ord k, Prim v) => Map k v -> [(k,v)]
toList (Map m) = I.toList m

-- | /O(n*log n)/ Create a map from a list of key-value pairs.
-- If the list contains more than one value for the same key,
-- the last value is retained. If the keys in the argument are
-- in nondescending order, this algorithm runs in /O(n)/ time instead.
fromList :: (Prim k, Ord k) => [(k,v)] -> Map k v
fromList = Map . I.fromList

-- | /O(n*log n)/ This function has the same behavior as 'fromList'
-- regardless of whether or not the expected size is accurate. Additionally,
-- negative sizes are handled correctly. The expected size is used as the
-- size of the initially allocated buffer when building the 'Map'. If the
-- keys in the argument are in nondescending order, this algorithm runs
-- in /O(n)/ time.
fromListN :: (Prim k, Ord k)
  => Int -- ^ expected size of resulting 'Map'
  -> [(k,v)] -- ^ key-value pairs
  -> Map k v
fromListN n = Map . I.fromListN n

-- | /O(n*log n)/ This function has the same behavior as 'fromList',
-- but it combines values with the 'Semigroup' instances instead of
-- choosing the last occurrence.
fromListAppend :: (Prim k, Ord k, Semigroup v) => [(k,v)] -> Map k v
fromListAppend = Map . I.fromListAppend

-- | /O(n*log n)/ This function has the same behavior as 'fromListN',
-- but it combines values with the 'Semigroup' instances instead of
-- choosing the last occurrence.
fromListAppendN :: (Prim k, Ord k, Semigroup v)
  => Int -- ^ expected size of resulting 'Map'
  -> [(k,v)] -- ^ key-value pairs
  -> Map k v
fromListAppendN n = Map . I.fromListAppendN n

-- | /O(1)/ The number of elements in the map.
size :: Map k v -> Int
size (Map m) = I.size m

-- | /O(n)/ Map over the values in the map.
map :: Prim k
  => (v -> w)
  -> Map k v
  -> Map k w
map f (Map m) = Map (I.map f m)

-- | /O(n)/ Drop elements for which the predicate returns 'Nothing'.
mapMaybe :: Prim k
  => (v -> Maybe w)
  -> Map k v
  -> Map k w
mapMaybe f (Map m) = Map (I.mapMaybe f m)

-- | /O(n)/ Drop elements for which the predicate returns 'Nothing'.
-- The predicate is given access to the key.
mapMaybeWithKey :: Prim k
  => (k -> v -> Maybe w)
  -> Map k v
  -> Map k w
mapMaybeWithKey f (Map m) = Map (I.mapMaybeWithKey f m)

-- | /O(n)/ Left monadic fold over the keys and values of the map. This fold
-- is strict in the accumulator.
foldlWithKeyM' :: (Monad m, Prim k)
  => (b -> k -> v -> m b)
  -> b
  -> Map k v
  -> m b
foldlWithKeyM' f b0 (Map m) = I.foldlWithKeyM' f b0 m

-- | /O(n)/ Right monadic fold over the keys and values of the map. This fold
-- is strict in the accumulator.
foldrWithKeyM' :: (Monad m, Prim k)
  => (k -> v -> b -> m b)
  -> b
  -> Map k v
  -> m b
foldrWithKeyM' f b0 (Map m) = I.foldrWithKeyM' f b0 m

-- | /O(n)/ Monadic left fold over the keys and values of the map with a strict
-- monoidal accumulator. The monoidal accumulator is appended to the left
-- after each reduction.
foldlMapWithKeyM' :: (Monad m, Monoid b, Prim k)
  => (k -> v -> m b)
  -> Map k v
  -> m b
foldlMapWithKeyM' f (Map m) = I.foldlMapWithKeyM' f m

-- | /O(n)/ Monadic right fold over the keys and values of the map with a strict
-- monoidal accumulator. The monoidal accumulator is appended to the right
-- after each reduction.
foldrMapWithKeyM' :: (Monad m, Monoid b, Prim k)
  => (k -> v -> m b) -- ^ reduction
  -> Map k v -- ^ map
  -> m b
foldrMapWithKeyM' f (Map m) = I.foldrMapWithKeyM' f m

-- | /O(n)/ Left fold over the keys and values with a strict accumulator.
foldlWithKey' :: Prim k
  => (b -> k -> v -> b) -- ^ reduction
  -> b -- ^ initial accumulator
  -> Map k v -- ^ map
  -> b
foldlWithKey' f b0 (Map m) = I.foldlWithKey' f b0 m

-- | /O(n)/ Right fold over the keys and values with a lazy accumulator.
foldrWithKey :: Prim k
  => (k -> v -> b -> b) -- ^ reduction
  -> b -- ^ initial accumulator
  -> Map k v -- ^ map
  -> b
foldrWithKey f b0 (Map m) = I.foldrWithKey f b0 m

-- | /O(n)/ Right fold over the keys and values with a strict accumulator.
foldrWithKey' :: Prim k
  => (k -> v -> b -> b) -- ^ reduction
  -> b -- ^ initial accumulator
  -> Map k v -- ^ map
  -> b
foldrWithKey' f b0 (Map m) = I.foldrWithKey' f b0 m

-- | /O(n)/ Fold over the keys and values of the map with a lazy monoidal
-- accumulator. This function does not have left and right variants since
-- the associativity required by a monoid instance means that both variants
-- would always produce the same result.
foldMapWithKey :: (Monoid b, Prim k)
  => (k -> v -> b)
  -> Map k v
  -> b
foldMapWithKey f (Map m) = I.foldMapWithKey f m

-- | /O(n)/ Fold over the keys and values of the map with a strict monoidal
-- accumulator. This function does not have left and right variants since
-- the associativity required by a monoid instance means that both variants
-- would always produce the same result.
foldMapWithKey' :: (Monoid b, Prim k)
  => (k -> v -> b)
  -> Map k v
  -> b
foldMapWithKey' f (Map m) = I.foldMapWithKey' f m

-- | /O(n*log n)/ Zip an array of keys with an array of values. If they are
-- not the same length, the longer one will be truncated to match the shorter
-- one. This function sorts and deduplicates the array of keys, preserving the
-- last value associated with each key. The argument arrays may not be
-- reused after being passed to this function.
--
-- This is by far the fastest way to create a map, since the functions backing it
-- are aggressively specialized. It internally uses a hybrid of mergesort and
-- insertion sort provided by the @primitive-sort@ package. It generates much
-- less garbage than any of the @fromList@ variants. 
unsafeFreezeZip :: (Ord k, Prim k)
  => MutablePrimArray s k
  -> MutableArray s v
  -> ST s (Map k v)
unsafeFreezeZip theKeys vals = fmap Map (I.unsafeFreezeZip theKeys vals)

-- | /O(1)/ Get the keys from the map.
keys :: Map k v -> Set k
keys (Map m) = Set (I.keys m)

intersectionWith :: (Prim k, Ord k)
  => (a -> b -> c)
  -> Map k a
  -> Map k b
  -> Map k c
intersectionWith f (Map a) (Map b) = Map (I.intersectionWith f a b)

restrict :: (Prim k, Ord k)
  => Map k v
  -> Set k
  -> Map k v
restrict (Map m) (Set s) = Map (I.restrict m s)

-- | /O(1)/ The values in a map. This is a zero-cost operation.
elems :: Map k v -> Array v
elems (Map m) = I.elems m

