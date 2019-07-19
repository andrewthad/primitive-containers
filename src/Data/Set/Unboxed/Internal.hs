{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Set.Unboxed.Internal
  ( Set(..)
  , toList
  , fromList
  ) where

import Prelude hiding (foldr)

import Data.Hashable (Hashable)
import Data.Primitive (Prim,PrimArray(..))
import Data.Semigroup (Semigroup)
import Data.Primitive.Unlifted.Class (PrimUnlifted(..))

import qualified Data.Foldable as F
import qualified Data.Hashable as H
import qualified Data.Semigroup as SG
import qualified Data.Set.Internal as I
import qualified GHC.Exts as E

-- | A set of elements.
newtype Set a = Set (I.Set PrimArray a)

instance (Prim a, Ord a) => Semigroup (Set a) where
  Set x <> Set y = Set (I.append x y)
  stimes = SG.stimesIdempotentMonoid
  sconcat xs = Set (I.concat (E.coerce (F.toList xs)))

instance (Prim a, Ord a) => Monoid (Set a) where
  mempty = Set I.empty
  mappend = (SG.<>)
  mconcat xs = Set (I.concat (E.coerce xs))

instance (Prim a, Eq a) => Eq (Set a) where
  Set x == Set y = I.equals x y

instance (Prim a, Ord a) => Ord (Set a) where
  compare (Set x) (Set y) = I.compare x y

instance (Hashable a, Prim a) => Hashable (Set a) where
  hashWithSalt s (Set arr) = I.liftHashWithSalt H.hashWithSalt s arr

instance PrimUnlifted (Set a) where
  type Unlifted (Set a) = E.ByteArray#
  {-# inline toUnlifted# #-}
  {-# inline fromUnlifted# #-}
  {-# inline writeUnliftedArray# #-}
  {-# inline readUnliftedArray# #-}
  {-# inline indexUnliftedArray# #-}
  toUnlifted# (Set (I.Set p)) = toUnlifted# p
  fromUnlifted# b# = Set (I.Set (PrimArray b#))
  writeUnliftedArray# a i s = E.writeByteArrayArray# a i (toUnlifted# s)
  readUnliftedArray# a i s0 = case E.readByteArrayArray# a i s0 of
    (# s1, x #) -> (# s1, fromUnlifted# x #)
  indexUnliftedArray# a i = fromUnlifted# (E.indexByteArrayArray# a i)

-- | The functions that convert a list to a 'Set' are asymptotically
-- better that using @'foldMap' 'singleton'@, with a cost of /O(n*log n)/
-- rather than /O(n^2)/. If the input list is sorted, even if duplicate
-- elements are present, the algorithm further improves to /O(n)/. The
-- fastest option available is calling 'fromListN' on a presorted list
-- and passing the correct size size of the resulting 'Set'. However, even
-- if an incorrect size is given to this function,
-- it will still correctly convert the list into a 'Set'.
instance (Prim a, Ord a) => E.IsList (Set a) where
  type Item (Set a) = a
  fromListN n = Set . I.fromListN n
  fromList = Set . I.fromList
  toList = toList

instance (Prim a, Show a) => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

-- | Convert a set to a list. The elements are given in ascending order.
toList :: Prim a => Set a -> [a]
toList (Set s) = I.toList s

-- | Convert a list to a set.
fromList :: (Ord a, Prim a) => [a] -> Set a
fromList xs = Set (I.fromList xs)


