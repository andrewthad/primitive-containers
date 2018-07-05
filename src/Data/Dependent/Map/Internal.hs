{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module Data.Dependent.Map.Internal
  ( Map
  , singleton
  , lookup
  , fromList
  , fromListN
  , appendRightBiased
  , toList
  , showsPrec
  , equals
  , compare
  , unsafeFreezeZip
  ) where

import Prelude hiding (lookup,showsPrec,compare)

import Data.Dependent.Map.Class (Universally,Apply,ApplyUniversally)
import Data.Primitive.Contiguous (Contiguous,Mutable,Element)
import Control.Monad.ST (ST,runST)
import Data.Proxy (Proxy(..))
import GHC.Exts (Any,coerce)
import Unsafe.Coerce (unsafeCoerce)
import Data.Exists (OrdForallPoly(..),EqForallPoly(..),DependentPair(..),ShowForall,ToSing)
import Data.Exists (ShowForeach,EqForeach,OrdForeach)
import Data.Semigroup (Semigroup)
import Data.Primitive.Sort (sortUniqueTaggedMutable)
import Data.Kind (Type)
import qualified Prelude as P
import qualified Data.Map.Internal as I
import qualified Data.Primitive.Contiguous as I
import qualified Data.Dependent.Map.Class as C
import qualified Data.Map.Internal as M

newtype Map karr varr (k :: u -> Type) (v :: u -> Type) = Map (M.Map karr varr (Apply k Any) (v Any))

singleton :: forall karr varr k v a.
     (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr))
  => k a -> v a -> Map karr varr k v
singleton k v = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ Map (M.singleton (wrapKey k) (wrapValue (Proxy :: Proxy v) (Proxy :: Proxy a) v))

lookup :: forall karr varr k v a.
     (OrdForallPoly k, Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr))
  => k a
  -> Map karr varr k v
  -> Maybe (v a)
{-# INLINABLE lookup #-}
lookup k (Map m) = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ case M.lookup (wrapKey k) m of
      Nothing -> Nothing
      Just v -> Just (unwrapValue (Proxy :: Proxy v) (Proxy :: Proxy a) v)

appendRightBiased :: forall karr varr k v.
     (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), OrdForallPoly k)
  => Map karr varr k v
  -> Map karr varr k v
  -> Map karr varr k v
appendRightBiased (Map m1) (Map m2) = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ Map (M.appendRightBiased m1 m2)

wrapKeyUnapplied :: f k -> f Any
wrapKeyUnapplied = unsafeCoerce

wrapKey :: f k -> Apply f Any
wrapKey = unsafeCoerce

wrapValue :: Proxy v -> Proxy a -> v a -> v Any
wrapValue _ _ = unsafeCoerce

unwrapValue :: Proxy v -> Proxy a -> v Any -> v a
unwrapValue _ _ = unsafeCoerce

unsafeCoerceMutableKeyArray ::
     Mutable karr s (f Any)
  -> Mutable karr s (Apply f Any)
unsafeCoerceMutableKeyArray = unsafeCoerce

fromList ::
     (Contiguous karr, ApplyUniversally k (Element karr), Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), OrdForallPoly k)
  => [DependentPair k v]
  -> Map karr varr k v
fromList = fromListN 1

fromListN ::
     (Contiguous karr, ApplyUniversally k (Element karr), Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), OrdForallPoly k)
  => Int
  -> [DependentPair k v]
  -> Map karr varr k v
{-# INLINABLE fromListN #-}
fromListN n xs = runST $ do
  (ks,vs) <- mutableArraysFromPairs (max n 1) xs
  unsafeFreezeZip ks vs

-- | This function is really unsafe. The user needs to use unsafeCoerce to even use it.
unsafeFreezeZip :: forall karr varr k v s.
     (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), OrdForallPoly k)
  => Mutable karr s (k Any)
  -> Mutable varr s (v Any)
  -> ST s (Map karr varr k v)
{-# INLINABLE unsafeFreezeZip #-}
unsafeFreezeZip keys0 vals0 = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ fmap Map (M.unsafeFreezeZip (unsafeCoerceMutableKeyArray keys0) vals0)

mutableArraysFromPairs :: forall karr varr k v s.
     (Contiguous karr, ApplyUniversally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), OrdForallPoly k)
  => Int -- must be at least one
  -> [DependentPair k v]
  -> ST s (Mutable karr s (k Any), Mutable varr s (v Any))
{-# INLINABLE mutableArraysFromPairs #-}
mutableArraysFromPairs n xs = id
  $ C.applyUniversallyLifted (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ do
    let go :: Int
           -> Int
           -> Mutable karr s (k Any)
           -> Mutable varr s (v Any)
           -> [DependentPair k v]
           -> ST s (Int,Mutable karr s (k Any),Mutable varr s (v Any))
        go !ix !_ !ks !vs [] = return (ix,ks,vs)
        go !ix !len !ks !vs (DependentPair k v : ys) = if ix < len
          then do
            I.write ks ix (wrapKeyUnapplied k)
            I.write vs ix (wrapValue (Proxy :: Proxy v) Proxy v)
            go (ix + 1) len ks vs ys
          else do
            let len' = len * 2
            ks' <- I.new len'
            vs' <- I.new len'
            I.copyMutable ks' 0 ks 0 len
            I.copyMutable vs' 0 vs 0 len
            I.write ks' ix (wrapKeyUnapplied k)
            I.write vs' ix (wrapValue (Proxy :: Proxy v) Proxy v)
            go (ix + 1) len' ks' vs' ys
    ks0 <- I.new n
    vs0 <- I.new n
    (len,ks',vs') <- go 0 n ks0 vs0 xs
    ksFinal <- I.resize ks' len
    vsFinal <- I.resize vs' len
    return (ksFinal,vsFinal)

foldrWithKey :: forall karr varr k v b.
     (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr))
  => (forall a. k a -> v a -> b -> b)
  -> b
  -> Map karr varr k v
  -> b
foldrWithKey f z (Map m) = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ M.foldrWithKey (unsafeCoerceRightFoldFunction f) z m

toList :: 
     (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr))
  => Map karr varr k v
  -> [DependentPair k v]
toList = foldrWithKey (\k v xs -> DependentPair k v : xs) []

unsafeCoerceRightFoldFunction :: 
     (forall a. k a -> v a -> b -> b)
  -> Apply k Any -> v Any -> b -> b
unsafeCoerceRightFoldFunction = unsafeCoerce

showsPrec :: (Contiguous karr, Universally k (Element karr), ShowForall k, ShowForeach v, ToSing k, Contiguous varr, ApplyUniversally v (Element varr))
  => Int -> Map karr varr k v -> ShowS
showsPrec p xs = showParen (p > 10) $
  showString "fromList " . shows (toList xs)

equals :: (Contiguous karr, Universally k (Element karr), EqForallPoly k, EqForeach v, ToSing k, Contiguous varr, ApplyUniversally v (Element varr))
  => Map karr varr k v
  -> Map karr varr k v
  -> Bool
equals a b = toList a == toList b

compare :: (Contiguous karr, Universally k (Element karr), OrdForallPoly k, OrdForeach v, ToSing k, Contiguous varr, ApplyUniversally v (Element varr))
  => Map karr varr k v
  -> Map karr varr k v
  -> Ordering
compare a b = P.compare (toList a) (toList b)

-- data Map karr varr f = Map !(karr (f Any)) !(varr (Value f Any))
-- 
-- keyToAny :: f a -> f Any
-- keyToAny = unsafeCoerce
-- 
-- valueToAny :: Proxy f -> Proxy a -> Value f a -> Value f Any
-- valueToAny _ _ = unsafeCoerce
-- 
-- 
-- singleton :: forall karr varr f k.
--      (Contiguous karr, Universally f (Element karr), Contiguous varr, ValueUniversally f (Element varr))
--   => f k -> Value f k -> Map karr varr f
-- singleton k v = id
--   $ C.universally (Proxy :: Proxy f) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
--   $ C.valueUniversally (Proxy :: Proxy f) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
--   $ Map
--     ( runST $ do
--         arr <- I.new 1
--         I.write arr 0 (keyToAny k)
--         I.unsafeFreeze arr
--     )
--     ( runST $ do
--         arr <- I.new 1
--         I.write arr 0 (valueToAny (Proxy :: Proxy f) (Proxy :: Proxy k) v)
--         I.unsafeFreeze arr
--     )

