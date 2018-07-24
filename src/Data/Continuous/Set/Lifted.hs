{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}
module Data.Continuous.Set.Lifted
  ( Set
  , Inclusivity(..)
  , singleton
  , member
  , empty
  , universe
  , null
  , universal
  ) where

import Prelude hiding (lookup,map,foldr,negate,null)

import Data.Semigroup (Semigroup)
import Data.Primitive (Array)
import Data.Continuous.Set.Internal (Inclusivity(..))
import qualified Data.Semigroup as SG
import qualified Data.Continuous.Set.Internal as I

-- | A diet set. Currently, the data constructor for this type is
-- exported. Please do not use it. It will be moved to an internal
-- module at some point.
newtype Set a = Set (I.Set Array a)

-- | /O(1)/ Create a continuous interval set with a single interval.
singleton :: Ord a
  => Maybe (Inclusivity,a) -- ^ lower bound
  -> Maybe (Inclusivity,a) -- ^ upper bound
  -> Set a
singleton lo hi = Set (I.singleton lo hi)

-- | /O(log n)/ Returns @True@ if the element is a member of the continuous
-- interval set.
member :: Ord a => a -> Set a -> Bool
member a (Set s) = I.member a s

empty :: Set a
empty = Set I.empty

universe :: Set a
universe = Set I.universe

null :: Set a -> Bool
null (Set s) = I.null s

universal :: Set a -> Bool
universal (Set s) = I.universal s

instance Show a => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

instance Eq a => Eq (Set a) where
  Set x == Set y = I.equals x y

instance (Ord a) => Semigroup (Set a) where
  Set x <> Set y = Set (I.append x y)

instance (Ord a, Enum a) => Monoid (Set a) where
  mempty = Set I.empty
  mappend = (SG.<>)

