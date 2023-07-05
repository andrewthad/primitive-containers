{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}
module Data.Map.Unboxed.Unlifted
  ( Map
  , empty
  , singleton
  , lookup
  , size
    -- * Transform
  , map
  , mapLifted
  , mapMaybe
  , mapMaybeP
  , mapMaybeWithKey
  , adjustMany
    -- * Folds
  , foldlWithKey'
  , foldrWithKey'
  , foldMapWithKey
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
  , fromSetP
    -- * Array Conversion
  , unsafeFreezeZip
  ) where

import Prelude hiding (lookup,map,traverse)

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (ST)
import Data.Primitive (PrimArray,MutablePrimArray)
import Data.Primitive.Types (Prim)
import Data.Primitive.Unlifted.Array (UnliftedArray_,UnliftedArray,MutableUnliftedArray)
import Data.Primitive.Unlifted.Class (Unlifted,PrimUnlifted)
import Data.Semigroup (Semigroup)
import Data.Set.Unboxed.Internal (Set(..))

import qualified Data.Map.Unboxed.Lifted as MUL
import qualified Data.Map.Internal as I
import qualified Data.Semigroup as SG
import qualified GHC.Exts as E

-- | A map from keys @k@ to values @v@. The key type must have a
-- 'Prim' instance.
newtype Map k v = Map (I.Map PrimArray (UnliftedArray_ (Unlifted v)) k v)

instance (Prim k, Ord k, PrimUnlifted v, Semigroup v) => Semigroup (Map k v) where
  Map x <> Map y = Map (I.append x y)

instance (Prim k, Ord k, PrimUnlifted v, Semigroup v) => Monoid (Map k v) where
  mempty = Map I.empty
  mappend = (SG.<>)
  mconcat = Map . I.concat . E.coerce

instance (Prim k, Eq k, PrimUnlifted v, Eq v) => Eq (Map k v) where
  Map x == Map y = I.equals x y

instance (Prim k, Ord k, PrimUnlifted v, Ord v) => Ord (Map k v) where
  compare (Map x) (Map y) = I.compare x y

instance (Prim k, Ord k, PrimUnlifted v) => E.IsList (Map k v) where
  type Item (Map k v) = (k,v)
  fromListN n = Map . I.fromListN n
  fromList = Map . I.fromList
  toList (Map s) = I.toList s

instance (Prim k, Show k, PrimUnlifted v, Show v) => Show (Map k v) where
  showsPrec p (Map s) = I.showsPrec p s

-- | The empty diet map.
empty :: Map k v
empty = Map I.empty

-- | /O(log n)/ Lookup the value at a key in the map.
lookup :: (Prim k, Ord k, PrimUnlifted v) => k -> Map k v -> Maybe v
lookup a (Map s) = I.lookup a s

-- | /O(1)/ Create a map with a single element.
singleton :: (Prim k, PrimUnlifted v) => k -> v -> Map k v
singleton k v = Map (I.singleton k v)

-- | /O(n*log n)/ Create a map from a list of key-value pairs.
-- If the list contains more than one value for the same key,
-- the last value is retained. If the keys in the argument are
-- in nondescending order, this algorithm runs in /O(n)/ time instead.
fromList :: (Prim k, Ord k, PrimUnlifted v) => [(k,v)] -> Map k v
fromList = Map . I.fromList

-- | /O(n*log n)/ This function has the same behavior as 'fromList'
-- regardless of whether or not the expected size is accurate. Additionally,
-- negative sizes are handled correctly. The expected size is used as the
-- size of the initially allocated buffer when building the 'Map'. If the
-- keys in the argument are in nondescending order, this algorithm runs
-- in /O(n)/ time.
fromListN :: (Prim k, Ord k, PrimUnlifted v)
  => Int -- ^ expected size of resulting 'Map'
  -> [(k,v)] -- ^ key-value pairs
  -> Map k v
fromListN n = Map . I.fromListN n

-- | /O(n*log n)/ This function has the same behavior as 'fromList',
-- but it combines values with the 'Semigroup' instances instead of
-- choosing the last occurrence.
fromListAppend :: (Prim k, Ord k, PrimUnlifted v, Semigroup v) => [(k,v)] -> Map k v
fromListAppend = Map . I.fromListAppend

-- | /O(n*log n)/ This function has the same behavior as 'fromListN',
-- but it combines values with the 'Semigroup' instances instead of
-- choosing the last occurrence.
fromListAppendN :: (Prim k, Ord k, PrimUnlifted v, Semigroup v)
  => Int -- ^ expected size of resulting 'Map'
  -> [(k,v)] -- ^ key-value pairs
  -> Map k v
fromListAppendN n = Map . I.fromListAppendN n

-- | /O(n)/ Build a map from a set. This function is uses the underlying
-- array that backs the set as the array for the keys. It constructs the
-- values by apply the given function to each key.
fromSet :: (Prim k, PrimUnlifted v)
  => (k -> v)
  -> Set k
  -> Map k v
{-# INLINE fromSet #-}
fromSet f (Set s) = Map (I.fromSet f s)

-- | /O(n)/ Build a map from a set. This function is uses the underlying
-- array that backs the set as the array for the keys. It constructs the
-- values by apply the given function to each key. The function can perform
-- primitive monadic effects.
fromSetP :: (PrimMonad m, Prim k, PrimUnlifted v)
  => (k -> m v)
  -> Set k
  -> m (Map k v)
{-# INLINE fromSetP #-}
fromSetP f (Set s) = fmap Map (I.fromSetP f s)

-- | /O(1)/ The number of elements in the map.
size :: PrimUnlifted v => Map k v -> Int
size (Map m) = I.size m

-- | /O(n)/ Map over the values in the map.
map :: (Prim k, PrimUnlifted v, PrimUnlifted w)
  => (v -> w)
  -> Map k v
  -> Map k w
map f (Map m) = Map (I.map f m)

-- | /O(n)/ Map over the values in the map. The resulting map contains
--   lifted values.
mapLifted :: (Prim k, PrimUnlifted v)
  => (v -> w)
  -> Map k v
  -> MUL.Map k w
mapLifted f (Map m) = MUL.Map (I.map f m)

-- | /O(n)/ Drop elements for which the predicate returns 'Nothing'.
mapMaybe :: (Prim k, PrimUnlifted v, PrimUnlifted w)
  => (v -> Maybe w)
  -> Map k v
  -> Map k w
{-# INLINE mapMaybe #-}
mapMaybe f (Map m) = Map (I.mapMaybe f m)

-- | /O(n)/ Drop elements for which the predicate returns 'Nothing'.
mapMaybeP :: (PrimMonad m, Prim k, PrimUnlifted v, PrimUnlifted w)
  => (v -> m (Maybe w))
  -> Map k v
  -> m (Map k w)
{-# INLINE mapMaybeP #-}
mapMaybeP f (Map m) = fmap Map (I.mapMaybeP f m)

-- | /O(n)/ Drop elements for which the predicate returns 'Nothing'.
-- The predicate is given access to the key.
mapMaybeWithKey :: (Prim k, PrimUnlifted v, PrimUnlifted w)
  => (k -> v -> Maybe w)
  -> Map k v
  -> Map k w
mapMaybeWithKey f (Map m) = Map (I.mapMaybeWithKey f m)

-- | Update the values at any number of keys. This is done
-- on in a buffer without building intermediate maps. Example use:
--
-- > adjustMany
-- >   (\adjust -> do
-- >     adjust 2 (\x -> pure (x + 1))
-- >     adjust 3 (\_ -> pure 42)
-- >   ) myMap
--
-- This increments by 1 the value associated with key 2. Then,
-- it replaces with 42 the value associated with key 3.
adjustMany :: (Prim k, PrimUnlifted v, PrimMonad m, Ord k)
  => ((k -> (v -> m v) -> m ()) -> m a) -- ^ Modification-applying function
  -> Map k v -- ^ Map
  -> m (Map k v, a)
adjustMany f (Map m) = do
  (r,a) <- I.adjustMany f m
  pure (Map r, a)

-- | /O(n)/ Left monadic fold over the keys and values of the map. This fold
-- is strict in the accumulator.
foldlWithKeyM' :: (Monad m, Prim k, PrimUnlifted v)
  => (b -> k -> v -> m b) -- ^ reduction
  -> b -- ^ initial accumulator
  -> Map k v -- ^ map
  -> m b
foldlWithKeyM' f b0 (Map m) = I.foldlWithKeyM' f b0 m

-- | /O(n)/ Right monadic fold over the keys and values of the map. This fold
-- is strict in the accumulator.
foldrWithKeyM' :: (Monad m, Prim k, PrimUnlifted v)
  => (k -> v -> b -> m b) -- ^ reduction
  -> b -- ^ initial accumulator
  -> Map k v -- ^ map
  -> m b
foldrWithKeyM' f b0 (Map m) = I.foldrWithKeyM' f b0 m

-- | /O(n)/ Monadic left fold over the keys and values of the map with a strict
-- monoidal accumulator. The monoidal accumulator is appended to the left
-- after each reduction.
foldlMapWithKeyM' :: (Monad m, Monoid b, Prim k, PrimUnlifted v)
  => (k -> v -> m b) -- ^ reduction
  -> Map k v -- ^ map
  -> m b
foldlMapWithKeyM' f (Map m) = I.foldlMapWithKeyM' f m

-- | /O(n)/ Monadic right fold over the keys and values of the map with a strict
-- monoidal accumulator. The monoidal accumulator is appended to the right
-- after each reduction.
foldrMapWithKeyM' :: (Monad m, Monoid b, Prim k, PrimUnlifted v)
  => (k -> v -> m b) -- ^ reduction
  -> Map k v -- ^ map
  -> m b
foldrMapWithKeyM' f (Map m) = I.foldrMapWithKeyM' f m

-- | /O(n)/ Traverse the values of the map.
traverse :: (Applicative m, Prim k, PrimUnlifted v, PrimUnlifted b)
  => (v -> m b)
  -> Map k v
  -> m (Map k b)
traverse f (Map m) = Map <$> (I.traverse f m)

-- | /O(n)/ traversal over the values in the map, using the keys.
traverseWithKey :: (Applicative f, Prim k, PrimUnlifted v, PrimUnlifted b)
  => (k -> v -> f b)
  -> Map k v
  -> f (Map k b)
traverseWithKey f (Map m) = Map <$> I.traverseWithKey f m

-- | /O(n)/ Traverse the keys and values of the map from left to right.
traverseWithKey_ :: (Monad m, Prim k, PrimUnlifted v)
  => (k -> v -> m b) -- ^ reduction
  -> Map k v -- ^ map
  -> m ()
traverseWithKey_ f (Map m) = I.traverseWithKey_ f m


-- | /O(n)/ Fold over the keys and values of the map with a monoidal
-- accumulator. This function does not have left and right variants since
-- the associativity required by a monoid instance means that both variants
-- would always produce the same result.
foldMapWithKey :: (Monoid b, Prim k, PrimUnlifted v)
  => (k -> v -> b) -- ^ reduction 
  -> Map k v -- ^ map
  -> b
foldMapWithKey f (Map m) = I.foldMapWithKey f m

-- | /O(n)/ Fold over the keys and values of the map with a strict monoidal
-- accumulator. This function does not have left and right variants since
-- the associativity required by a monoid instance means that both variants
-- would always produce the same result.
foldMapWithKey' :: (Monoid b, Prim k, PrimUnlifted v)
  => (k -> v -> b) -- ^ reduction 
  -> Map k v -- ^ map
  -> b
foldMapWithKey' f (Map m) = I.foldMapWithKey' f m

-- | /O(n)/ Left fold over the keys and values with a strict accumulator.
foldlWithKey' :: (Prim k, PrimUnlifted v)
  => (b -> k -> v -> b) -- ^ reduction
  -> b -- ^ initial accumulator
  -> Map k v -- ^ map
  -> b
foldlWithKey' f b0 (Map m) = I.foldlWithKey' f b0 m

-- | /O(n)/ Right fold over the keys and values with a strict accumulator.
foldrWithKey' :: (Prim k, PrimUnlifted v)
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
unsafeFreezeZip :: (Ord k, Prim k, PrimUnlifted v)
  => MutablePrimArray s k
  -> MutableUnliftedArray s v
  -> ST s (Map k v)
unsafeFreezeZip keys vals = fmap Map (I.unsafeFreezeZip keys vals)

