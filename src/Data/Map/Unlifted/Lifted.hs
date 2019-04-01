{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Map.Unlifted.Lifted
  ( Map(..)
  , empty
  , singleton
  , lookup
  , size
  , map
  , mapMaybe
  , mapMaybeWithKey
  , union
    -- * Folds
  , foldlWithKey'
  , foldrWithKey'
  , foldMapWithKey'
    -- * Monadic Folds
  , foldlWithKeyM'
  , foldrWithKeyM'
  , foldlMapWithKeyM'
  , foldrMapWithKeyM'
    -- * Traversals
  , traverse
  , traverseWithKey
  , traverseWithKey_
    -- * List Conversion
  , fromList
  , fromListAppend
  , fromListN
  , fromListAppendN
  , fromSet
    -- * Array Conversion
  , unsafeFreezeZip
  ) where

import Prelude hiding (lookup,map,traverse)

import Control.Monad.ST (ST)
import Data.Semigroup (Semigroup)
import Data.Primitive.UnliftedArray (PrimUnlifted,UnliftedArray,MutableUnliftedArray)
import Data.Primitive (Array,MutableArray)
import Data.Set.Unlifted.Internal (Set(..))
import qualified GHC.Exts as E
import qualified Data.Semigroup as SG
import qualified Data.Map.Internal as I

-- | A map from keys @k@ to values @v@. The key type must have a
--   'PrimUnlifted' instance and the value type must have a 'Prim'
--   instance.
--
--   The data constructor for this type should not be exported.
--   I am working on this.
newtype Map k v = Map (I.Map UnliftedArray Array k v)

instance (PrimUnlifted k, Ord k, Semigroup v) => Semigroup (Map k v) where
  Map x <> Map y = Map (I.append x y)

instance (PrimUnlifted k, Ord k, Semigroup v) => Monoid (Map k v) where
  mempty = Map I.empty
  mappend = (SG.<>)
  mconcat = Map . I.concat . E.coerce

instance (PrimUnlifted k, Eq k, Eq v) => Eq (Map k v) where
  Map x == Map y = I.equals x y

instance (PrimUnlifted k, Ord k, Ord v) => Ord (Map k v) where
  compare (Map x) (Map y) = I.compare x y

instance (PrimUnlifted k, Ord k) => E.IsList (Map k v) where
  type Item (Map k v) = (k,v)
  fromListN n = Map . I.fromListN n
  fromList = Map . I.fromList
  toList (Map s) = I.toList s

instance (PrimUnlifted k, Show k, Show v) => Show (Map k v) where
  showsPrec p (Map s) = I.showsPrec p s

-- | /O(log n)/ Lookup the value at a key in the map.
lookup :: (PrimUnlifted k, Ord k) => k -> Map k v -> Maybe v
lookup a (Map s) = I.lookup a s

-- | The empty diet map.
empty :: Map k v
empty = Map I.empty

-- | /O(1)/ Create a map with a single element.
singleton :: PrimUnlifted k => k -> v -> Map k v
singleton k v = Map (I.singleton k v)

-- | /O(n*log n)/ Create a map from a list of key-value pairs.
-- If the list contains more than one value for the same key,
-- the last value is retained. If the keys in the argument are
-- in nondescending order, this algorithm runs in /O(n)/ time instead.
fromList :: (PrimUnlifted k, Ord k) => [(k,v)] -> Map k v
fromList = Map . I.fromList

-- | /O(n*log n)/ This function has the same behavior as 'fromList'
-- regardless of whether or not the expected size is accurate. Additionally,
-- negative sizes are handled correctly. The expected size is used as the
-- size of the initially allocated buffer when building the 'Map'. If the
-- keys in the argument are in nondescending order, this algorithm runs
-- in /O(n)/ time.
fromListN :: (PrimUnlifted k, Ord k)
  => Int -- ^ expected size of resulting 'Map'
  -> [(k,v)] -- ^ key-value pairs
  -> Map k v
fromListN n = Map . I.fromListN n

-- | /O(n*log n)/ This function has the same behavior as 'fromList',
-- but it combines values with the 'Semigroup' instances instead of
-- choosing the last occurrence.
fromListAppend :: (PrimUnlifted k, Ord k, Semigroup v) => [(k,v)] -> Map k v
fromListAppend = Map . I.fromListAppend

-- | /O(n*log n)/ This function has the same behavior as 'fromListN',
-- but it combines values with the 'Semigroup' instances instead of
-- choosing the last occurrence.
fromListAppendN :: (PrimUnlifted k, Ord k, Semigroup v)
  => Int -- ^ expected size of resulting 'Map'
  -> [(k,v)] -- ^ key-value pairs
  -> Map k v
fromListAppendN n = Map . I.fromListAppendN n

-- | /O(n)/ Build a map from a set. This function is uses the underlying
-- array that backs the set as the array for the keys. It constructs the
-- values by applying the given function to each key.
fromSet :: PrimUnlifted k
  => (k -> v)
  -> Set k
  -> Map k v
fromSet f (Set s) = Map (I.fromSet f s)

-- | /O(1)/ The number of elements in the map.
size :: Map k v -> Int
size (Map m) = I.size m

-- | /O(n)/ Map over the values in the map.
map :: PrimUnlifted k
  => (v -> w)
  -> Map k v
  -> Map k w
map f (Map m) = Map (I.map f m)

-- | /O(n)/ Drop elements for which the predicate returns 'Nothing'.
mapMaybe :: PrimUnlifted k
  => (v -> Maybe w)
  -> Map k v
  -> Map k w
mapMaybe f (Map m) = Map (I.mapMaybe f m)

-- | /O(n)/ Drop elements for which the predicate returns 'Nothing'.
-- The predicate is given access to the key.
mapMaybeWithKey :: PrimUnlifted k
  => (k -> v -> Maybe w)
  -> Map k v
  -> Map k w
mapMaybeWithKey f (Map m) = Map (I.mapMaybeWithKey f m)

-- | /O(n)/ traversal over the values in the map.
traverse :: (Applicative f, PrimUnlifted k)
  => (v -> f b)
  -> Map k v
  -> f (Map k b)
traverse f (Map m) = Map <$> I.traverse f m

-- | /O(n)/ traversal over the values in the map, using the keys.
traverseWithKey :: (Applicative f, PrimUnlifted k)
  => (k -> v -> f b)
  -> Map k v
  -> f (Map k b)
traverseWithKey f (Map m) = Map <$> I.traverseWithKey f m

-- | /O(n)/ like 'traverseWithKey', but discards the results.
traverseWithKey_ :: (Applicative f, PrimUnlifted k)
  => (k -> v -> f b)
  -> Map k v
  -> f ()
traverseWithKey_ f (Map m) = I.traverseWithKey_ f m

-- | /O(n)/ Left monadic fold over the keys and values of the map. This fold
-- is strict in the accumulator.
foldlWithKeyM' :: (Monad m, PrimUnlifted k)
  => (b -> k -> v -> m b) -- ^ reduction
  -> b -- ^ initial accumulator
  -> Map k v -- ^ map
  -> m b
foldlWithKeyM' f b0 (Map m) = I.foldlWithKeyM' f b0 m

-- | /O(n)/ Right monadic fold over the keys and values of the map. This fold
-- is strict in the accumulator.
foldrWithKeyM' :: (Monad m, PrimUnlifted k)
  => (k -> v -> b -> m b) -- ^ reduction
  -> b -- ^ initial accumulator
  -> Map k v -- ^ map
  -> m b
foldrWithKeyM' f b0 (Map m) = I.foldrWithKeyM' f b0 m

-- | /O(n)/ Monadic left fold over the keys and values of the map with a strict
-- monoidal accumulator. The monoidal accumulator is appended to the left
-- after each reduction.
foldlMapWithKeyM' :: (Monad m, Monoid b, PrimUnlifted k)
  => (k -> v -> m b) -- ^ reduction
  -> Map k v -- ^ map
  -> m b
foldlMapWithKeyM' f (Map m) = I.foldlMapWithKeyM' f m

-- | /O(n)/ Monadic right fold over the keys and values of the map with a strict
-- monoidal accumulator. The monoidal accumulator is appended to the right
-- after each reduction.
foldrMapWithKeyM' :: (Monad m, Monoid b, PrimUnlifted k)
  => (k -> v -> m b) -- ^ reduction
  -> Map k v -- ^ map
  -> m b
foldrMapWithKeyM' f (Map m) = I.foldrMapWithKeyM' f m

-- | /O(n)/ Fold over the keys and values of the map with a strict monoidal
-- accumulator. This function does not have left and right variants since
-- the associativity required by a monoid instance means that both variants
-- would always produce the same result.
foldMapWithKey' :: (Monoid b, PrimUnlifted k)
  => (k -> v -> b) -- ^ reduction 
  -> Map k v -- ^ map
  -> b
foldMapWithKey' f (Map m) = I.foldMapWithKey' f m

-- | /O(n)/ Left fold over the keys and values with a strict accumulator.
foldlWithKey' :: PrimUnlifted k
  => (b -> k -> v -> b) -- ^ reduction
  -> b -- ^ initial accumulator
  -> Map k v -- ^ map
  -> b
foldlWithKey' f b0 (Map m) = I.foldlWithKey' f b0 m

-- | /O(n)/ Right fold over the keys and values with a strict accumulator.
foldrWithKey' :: PrimUnlifted k
  => (k -> v -> b -> b) -- ^ reduction
  -> b -- ^ initial accumulator
  -> Map k v -- ^ map
  -> b
foldrWithKey' f b0 (Map m) = I.foldrWithKey' f b0 m

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
unsafeFreezeZip :: (Ord k, PrimUnlifted k)
  => MutableUnliftedArray s k
  -> MutableArray s v
  -> ST s (Map k v)
unsafeFreezeZip keys vals = fmap Map (I.unsafeFreezeZip keys vals)

-- | /O(n+m)/ The expression (@'union' t1 t2@) takes the left-biased union
-- of @t1@ and @t2@. It prefers @t1@ when duplicate keys are encountered.
union :: (Ord k, PrimUnlifted k) => Map k v -> Map k v -> Map k v
union (Map a) (Map b) = Map (I.appendWith const a b)

