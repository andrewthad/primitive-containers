{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}
module Data.Diet.Unbounded.Set.Lifted
  ( Set
  , singleton
  , member
  ) where

import Data.Semigroup (Semigroup)
import Data.Primitive (Array)
import qualified GHC.Exts as E
import qualified Data.Semigroup as SG
import qualified Data.Diet.Unbounded.Set.Internal as I

newtype Set a = Set (I.Set Array a)

instance Eq a => Eq (Set a) where
  Set x == Set y = I.equals x y

instance (Ord a, Enum a) => Semigroup (Set a) where
  Set x <> Set y = Set (I.append x y)

instance (Ord a, Enum a) => Monoid (Set a) where
  mempty = Set (I.empty)
  mappend = (SG.<>)

instance Show a => Show (Set a) where
  showsPrec p (Set s) = I.showsPrec p s

-- | /O(1)/ Create an unbounded diet set with a single element.
singleton :: Ord a
  => Maybe a -- ^ lower inclusive bound, @Nothing@ means @-∞@
  -> Maybe a -- ^ upper inclusive bound, @Nothing@ means @+∞@
  -> Set a
singleton lo hi = Set (I.singleton lo hi)

-- | /O(log n)/ Returns @True@ if the element is a member of the diet set.
member :: Ord a => a -> Set a -> Bool
member a (Set s) = I.member a s



