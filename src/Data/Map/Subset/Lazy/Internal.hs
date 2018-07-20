{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Map.Subset.Lazy.Internal
  ( Map
  , lookup
  , empty
  , singleton
  , toList
  , fromList
  ) where

import Prelude hiding (lookup,concat)

import Data.Primitive.Contiguous (Contiguous,Element)
import Data.Set.Internal (Set(..))
import Data.Bifunctor (first)
import Data.Semigroup (Semigroup,(<>),First(..))

import qualified Data.Primitive.Contiguous as A
import qualified Data.Set.Internal as S
import qualified Data.Set.Lifted.Internal as SL
import qualified Data.Semigroup as SG
import qualified Prelude as P
import qualified Data.Foldable as F

-- There are two invariants for Map.
--
-- 1. The children of any Map may only contain keys that are
--    greater than the key in their parent.
-- 2. A parent's two children must not be equal.
--
-- Unlike the strict variant, which imposes an Eq constraint on
-- values, the lazy variant is able to have a Functor instance.
data Map k v
  = MapElement k (Map k v) (Map k v)
  | MapValue v
  | MapEmpty
  deriving (Functor,Eq,Ord)

instance (Semigroup v, Eq v, Ord k) => Semigroup (Map k v) where
  (<>) = append

instance (Semigroup v, Eq v, Ord k) => Monoid (Map k v) where
  mempty = empty
  mappend = (SG.<>)
  -- mconcat = concat 

instance (Show k, Show v) => Show (Map k v) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (P.map (first SL.Set) (toList xs))

toList :: (Contiguous arr, Element arr k)
  => Map k v
  -> [(Set arr k,v)]
toList = foldrWithKey (\k v xs -> (k,v) : xs) []

fromList :: (Contiguous arr, Element arr k, Ord k)
  => [(Set arr k,v)]
  -> Map k v
fromList = fmap getFirst . concat . P.map (\(s,v) -> singleton s (First v))

concat :: (Ord k,Semigroup v)
  => [Map k v]
  -> Map k v
concat = F.foldl' (\r x -> append r x) empty

foldrWithKey :: (Contiguous arr, Element arr k)
  => (Set arr k -> v -> b -> b)
  -> b
  -> Map k v
  -> b
foldrWithKey f b0 = go 0 [] b0 where
  go !_ !_ b MapEmpty = b
  go !n !xs b (MapValue v) = f (Set (A.unsafeFromListReverseN n xs)) v b
  go !n !xs b (MapElement k present absent) =
    go (n + 1) (k : xs) (go n xs b absent) present

empty :: Map k v
empty = MapEmpty

singleton :: (Contiguous arr, Element arr k)
  => Set arr k
  -> v
  -> Map k v
singleton s v = S.foldr (\k m -> MapElement k m empty) (MapValue v) s
  
lookup :: forall arr k v. (Ord k, Contiguous arr, Element arr k)
  => Set arr k
  -> Map k v
  -> Maybe v
{-# INLINABLE lookup #-}
lookup (Set arr) = go 0 where
  !sz = A.size arr
  go :: Int -> Map k v -> Maybe v
  go !_ MapEmpty = Nothing
  go !_ (MapValue v) = Just v
  go !ix (MapElement element present absent) =
    choose ix element present absent
  choose :: Int -> k -> Map k v -> Map k v -> Maybe v
  choose !ix element present absent = if ix < sz
    then 
      let (# k #) = A.index# arr ix
       in case compare k element of
            EQ -> go (ix + 1) present
            LT -> choose (ix + 1) element present absent
            GT -> go ix absent
    else followAbsent absent

followAbsent :: Map k v -> Maybe v
followAbsent (MapElement _ _ x) = followAbsent x
followAbsent (MapValue v) = Just v
followAbsent MapEmpty = Nothing

augment :: Eq k => (v -> v) -> v -> Map k v -> Map k v
augment _ v MapEmpty = MapValue v
augment f _ (MapValue x) = MapValue (f x)
augment f v (MapElement k present absent) =
  let present' = augment f v present
      absent' = augment f v absent
   in MapElement k present' absent'

append :: forall k v. (Semigroup v, Ord k) => Map k v -> Map k v -> Map k v
append = go where
  go :: Map k v -> Map k v -> Map k v
  go MapEmpty m = m
  go (MapValue x) (MapValue y) = MapValue (x <> y)
  go (MapValue x) MapEmpty = MapValue x
  go (MapValue x) (MapElement elemY presentY absentY) =
    augment (x SG.<>) x (MapElement elemY presentY absentY)
  go (MapElement elemX presentX absentX) MapEmpty =
    MapElement elemX presentX absentX
  go (MapElement elemX presentX absentX) (MapValue y) =
    augment (SG.<> y) y (MapElement elemX presentX absentX)
  go (MapElement elemX presentX absentX) (MapElement elemY presentY absentY) = case compare elemX elemY of
    EQ -> 
      let present = go presentX presentY
          absent = go absentX absentY
       in MapElement elemX present absent
    LT ->
      let present = go presentX (MapElement elemY presentY absentY)
          absent = go absentX (MapElement elemY presentY absentY)
       in MapElement elemX present absent
    GT ->
      let present = go (MapElement elemX presentX absentX) presentY
          absent = go (MapElement elemX presentX absentX) absentY
       in MapElement elemY present absent
      

