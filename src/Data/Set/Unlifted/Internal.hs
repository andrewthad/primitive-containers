{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Set.Unlifted.Internal
  ( Set(..)
  , toList
  , fromList
  ) where

import Prelude hiding (foldr)

import Data.Hashable (Hashable)
import Data.Primitive.Unlifted.Array (UnliftedArray,UnliftedArray_)
import Data.Primitive.Unlifted.Class (PrimUnlifted,Unlifted)
import Data.Primitive (Array)

import qualified Data.Foldable as F
import qualified Data.Hashable as H
import qualified Data.Semigroup as SG
import qualified Data.Set.Internal as I
import qualified GHC.Exts as E

newtype Set a = Set { getSet :: I.Set (UnliftedArray_ (Unlifted a)) a }

instance (Ord a, PrimUnlifted a) => Semigroup (Set a) where
  Set x <> Set y = Set (I.append x y)
  stimes = SG.stimesIdempotentMonoid
  sconcat xs = Set (I.concat (E.coerce (F.toList xs)))

instance (Hashable a, PrimUnlifted a) => Hashable (Set a) where
  hashWithSalt s (Set arr) = I.liftHashWithSalt H.hashWithSalt s arr

instance (PrimUnlifted a, Ord a) => Monoid (Set a) where
  mempty = Set I.empty
  mappend = (SG.<>)
  mconcat xs = Set (I.concat (E.coerce xs))

instance (PrimUnlifted a, Eq a) => Eq (Set a) where
  Set x == Set y = I.equals x y

instance (PrimUnlifted a, Ord a) => Ord (Set a) where
  compare (Set x) (Set y) = I.compare x y

-- | The functions that convert a list to a 'Set' are asymptotically
-- better that using @'foldMap' 'singleton'@, with a cost of /O(n*log n)/
-- rather than /O(n^2)/. If the input list is sorted, even if duplicate
-- elements are present, the algorithm further improves to /O(n)/. The
-- fastest option available is calling 'fromListN' on a presorted list
-- and passing the correct size size of the resulting 'Set'. However, even
-- if an incorrect size is given to this function,
-- it will still correctly convert the list into a 'Set'.
instance (PrimUnlifted a, Ord a) => E.IsList (Set a) where
  type Item (Set a) = a
  fromListN n = Set . I.fromListN n
  fromList = Set . I.fromList
  toList = toList

instance (PrimUnlifted a, Show a) => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

-- | /O(n)/ Convert a set to a list. The elements are given in ascending order.
toList :: PrimUnlifted a => Set a -> [a]
toList (Set s) = I.toList s

-- | /O(n*log n)/ Convert a list to a set.
fromList :: (PrimUnlifted a, Ord a) => [a] -> Set a
fromList = Set . I.fromList

