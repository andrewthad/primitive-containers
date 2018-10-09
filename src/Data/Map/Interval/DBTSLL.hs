{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Map.Interval.DBTSLL
  ( Map
  , pure
  , singleton
  , lookup
  , fromList
  ) where

import Prelude hiding (lookup,map,pure)

import Data.Semigroup (Semigroup)
import Data.Primitive.Array (Array)
import qualified Data.Semigroup as SG
import qualified Data.Map.Interval.DBTS.Internal as I
import qualified GHC.Exts as E

-- | A total interval map from keys @k@ to values @v@. The key type must be discrete
--   and bounded. This map is strict in the values.
newtype Map k v = Map (I.Map Array Array k v)

instance (Eq k, Eq v) => Eq (Map k v) where
  Map x == Map y = I.equals x y

-- instance (Ord k, Ord v) => Ord (Map k v) where
--   compare (Map x) (Map y) = I.compare x y

instance (Ord k, Semigroup v, Eq v) => Semigroup (Map k v) where
  Map x <> Map y = Map (I.union x y)

-- The redundant constraint is needed for GHC < 8.4
instance (Ord k, Bounded k, Enum k, Semigroup v, Monoid v, Eq v) => Monoid (Map k v) where
  mappend = (SG.<>) 
  mempty = Map I.empty
  mconcat = Map . I.concat . E.coerce

instance (Bounded k, Enum k, Show k, Show v) => Show (Map k v) where
  showsPrec p (Map m) = I.showsPrec p m

instance (Bounded k, Enum k, Ord k, Eq v, Monoid v) => E.IsList (Map k v) where
  type Item (Map k v) = (k,k,v)
  fromList xs = Map (I.fromList mempty xs)
  toList (Map m) = I.toList m

pure :: Bounded k => v -> Map k v
pure = Map . I.pure 

singleton :: (Bounded k, Enum k, Ord k, Eq v)
  => v -- ^ value outside of the interval
  -> k -- ^ lower bound
  -> k -- ^ upper bound
  -> v -- ^ value inside the interval
  -> Map k v
singleton def lo hi v = Map (I.singleton def lo hi v)

lookup :: Ord k => k -> Map k v -> v
lookup k (Map m) = I.lookup k m

-- | Create an interval map from a list of range-value triples. The first
--   argument is a default value used everywhere outside of the given
--   ranges. In the case of overlapping ranges, the leftmost value is
--   used.
fromList :: (Bounded k, Ord k, Enum k, Eq v)
  => v -- ^ value outside of the ranges
  -> [(k,k,v)] -- ^ low-high inclusive ranges with their corresponding values
  -> Map k v
fromList def xs = Map (I.fromList def xs)


