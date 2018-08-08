{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Dependent.Map.Internal
  ( Map(..)
  , empty
  , null
  , singleton
  , lookup
  , fromList
  , fromListN
  , map
  , mapMaybe
  , mapMaybeWithKey
  , appendRightBiased
  , append
  , toList
  , showsPrec
  , equals
  , compare
  , unsafeFreezeZip
  , toJSON
  , parseJSON
  , foldrWithKey
  , foldlWithKeyM'
  , foldMapWithKey
  , traverseWithKey_
  , size
  ) where

import Prelude hiding (lookup,showsPrec,compare,null,map)

import Data.Dependent.Map.Class (Universally,Apply,ApplyUniversally)
import Data.Primitive.Contiguous (Contiguous,Mutable,Element)
import Control.Monad.ST (ST,runST)
import Data.Proxy (Proxy(..))
import GHC.Exts (Any,coerce)
import Unsafe.Coerce (unsafeCoerce)
import Data.Exists (OrdForallPoly(..),EqForallPoly(..),DependentPair(..),ShowForall,ToSing)
import Data.Exists (ShowForeach,EqForeach,OrdForeach,ToJSONKeyForall,FromJSONForeach)
import Data.Exists (ToJSONForall,ToJSONKeyFunctionForall,ToJSONForeach)
import Data.Exists (FromJSONKeyExists,SemigroupForeach,Sing)
import Data.Semigroup (Semigroup)
import Data.Primitive.Sort (sortUniqueTaggedMutable)
import Data.Kind (Type)
import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Exists as EX
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import qualified Data.HashMap.Strict as HM
import qualified Prelude as P
import qualified Data.Map.Internal as I
import qualified Data.Primitive.Contiguous as I
import qualified Data.Dependent.Map.Class as C
import qualified Data.Map.Internal as M
import qualified Data.Foldable as F

newtype Map karr varr (k :: u -> Type) (v :: u -> Type) = Map (M.Map karr varr (Apply k Any) (v Any))

empty :: (Contiguous karr, Contiguous varr) => Map karr varr k v
empty = Map M.empty

null :: forall karr varr k v. (Contiguous varr) => Map karr varr k v -> Bool
null (Map m) = M.null m

singleton :: forall karr varr k v a.
     (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr))
  => k a -> v a -> Map karr varr k v
singleton k v = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ Map (M.singleton (wrapKey k) (wrapValue (Proxy :: Proxy v) (Proxy :: Proxy a) v))

toJSON :: forall karr varr k v.
     (ToJSONKeyForall k, ToJSONForeach v, ToSing k, Contiguous karr, Contiguous varr,ApplyUniversally v (Element varr),Universally k (Element karr))
  => Map karr varr k v
  -> AE.Value
toJSON (Map m) = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ case EX.toJSONKeyForall :: ToJSONKeyFunctionForall k of
      EX.ToJSONKeyValueForall toValue _ -> AE.Array $ V.fromListN
        ( M.size m )
        ( M.foldrWithKey
          ( \(C.Apply k) v xs -> AE.toJSON (toValue k,EX.toJSONForeach (EX.toSing k) v) : xs
          ) [] m
        )
      EX.ToJSONKeyTextForall toText _ -> AE.Object
        ( M.foldlWithKey'
          ( \hm (C.Apply k) v -> HM.insert (toText k) (EX.toJSONForeach (EX.toSing k) v) hm
          ) HM.empty m
        )

parseJSON :: forall karr varr k v.
     (FromJSONKeyExists k, ToSing k, OrdForallPoly k, FromJSONForeach v, Contiguous karr, Contiguous varr, ApplyUniversally v (Element varr),Universally k (Element karr),ApplyUniversally k (Element karr))
  => AE.Value
  -> AET.Parser (Map karr varr k v)
parseJSON theValue =
  case EX.fromJSONKeyExists :: AE.FromJSONKeyFunction (EX.Exists k) of
    AE.FromJSONKeyCoerce _ -> error "Data.Dependent.Map.Internal.fromJSON: this cannot happen"
    AE.FromJSONKeyText fromText -> AET.withObject "DependentMap"
      (fmap fromList . HM.foldrWithKey (f1 fromText) (return []))
      theValue
    AE.FromJSONKeyTextParser fromText -> AET.withObject "DependentMap"
      (fmap fromList . HM.foldrWithKey (f2 fromText) (return []))
      theValue
    AE.FromJSONKeyValue fromValue -> AET.withArray "DependentMap"
      (fmap fromList . F.foldlM (f3 fromValue) [])
      theValue
  where
  f1 :: (Text -> EX.Exists k) -> Text -> AE.Value -> AET.Parser [DependentPair k v] -> AET.Parser [DependentPair k v]
  f1 fromText keyText valRaw m = case fromText keyText of
    EX.Exists key -> do
      let keySing = EX.toSing key
      val <- EX.parseJSONForeach keySing valRaw
      dm <- m
      return (DependentPair key val : dm)
  f2 :: (Text -> AET.Parser (EX.Exists k)) -> Text -> AE.Value -> AET.Parser [DependentPair k v] -> AET.Parser [DependentPair k v]
  f2 fromText keyText valRaw m = do
    EX.Exists key <- fromText keyText
    let keySing = EX.toSing key
    val <- EX.parseJSONForeach keySing valRaw
    dm <- m
    return (DependentPair key val : dm)
  f3 :: (AE.Value -> AET.Parser (EX.Exists k)) -> [DependentPair k v] -> AE.Value -> AET.Parser [DependentPair k v]
  f3 fromValue dm pairRaw = do
    (keyRaw :: AE.Value,valRaw :: AE.Value) <- AE.parseJSON pairRaw
    EX.Exists key <- fromValue keyRaw
    let keySing = EX.toSing key
    val <- EX.parseJSONForeach keySing valRaw
    return (DependentPair key val : dm)


        

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

appendWith :: forall karr varr k v.
     (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), OrdForallPoly k, ToSing k)
  => (forall a. Sing a -> v a -> v a -> v a)
  -> Map karr varr k v
  -> Map karr varr k v
  -> Map karr varr k v
appendWith f (Map m1) (Map m2) = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ Map (M.appendKeyWith (\(C.Apply k) v1 v2 -> f (EX.toSing k) v1 v2) m1 m2)

append :: forall karr varr k v.
     (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), OrdForallPoly k, SemigroupForeach v, ToSing k)
  => Map karr varr k v
  -> Map karr varr k v
  -> Map karr varr k v
append = appendWith EX.appendForeach

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

foldMapWithKey :: forall karr varr k v m.
     (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), Monoid m)
  => (forall a. k a -> v a -> m)
  -> Map karr varr k v
  -> m
foldMapWithKey f (Map m) = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ M.foldMapWithKey (unsafeCoerceFoldMapFunction f) m

traverseWithKey_ :: forall karr varr k v m b.
     (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), Applicative m)
  => (forall a. k a -> v a -> m b)
  -> Map karr varr k v
  -> m ()
traverseWithKey_ f (Map m) = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ M.traverseWithKey_ (unsafeCoerceFoldMapFunction f) m

foldlWithKeyM' :: forall karr varr k v m b.
     (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), Monad m)
  => (forall a. b -> k a -> v a -> m b)
  -> b
  -> Map karr varr k v
  -> m b
foldlWithKeyM' f z (Map m) = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ M.foldlWithKeyM' (unsafeCoerceLeftFoldFunctionM f) z m

toList :: 
     (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr))
  => Map karr varr k v
  -> [DependentPair k v]
toList = foldrWithKey (\k v xs -> DependentPair k v : xs) []

unsafeCoerceMapMaybeWithKeyFunction ::
     (forall a. k a -> v a -> Maybe (w a))
  -> Apply k Any -> v Any -> Maybe (w Any)
unsafeCoerceMapMaybeWithKeyFunction = unsafeCoerce

unsafeCoerceLeftFoldFunctionM :: 
     (forall a. b -> k a -> v a -> m b)
  -> b -> Apply k Any -> v Any -> m b
unsafeCoerceLeftFoldFunctionM = unsafeCoerce

unsafeCoerceRightFoldFunction :: 
     (forall a. k a -> v a -> b -> b)
  -> Apply k Any -> v Any -> b -> b
unsafeCoerceRightFoldFunction = unsafeCoerce

unsafeCoerceFoldMapFunction :: 
     (forall a. k a -> v a -> m)
  -> Apply k Any -> v Any -> m
unsafeCoerceFoldMapFunction = unsafeCoerce

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

size :: forall karr varr k v. (Contiguous varr, ApplyUniversally v (Element varr)) => Map karr varr k v -> Int
size (Map m) = id
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ M.size m

map :: forall karr varr k v w. (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), ApplyUniversally w (Element varr))
  => (forall a. v a -> w a)
  -> Map karr varr k v
  -> Map karr varr k w
map f (Map m) = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy w) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ Map (M.map f m)

mapMaybe :: forall karr varr k v w. (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), ApplyUniversally w (Element varr))
  => (forall a. v a -> Maybe (w a))
  -> Map karr varr k v
  -> Map karr varr k w
mapMaybe f (Map m) = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy w) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ Map (M.mapMaybe f m)

mapMaybeWithKey :: forall karr varr k v w. (Contiguous karr, Universally k (Element karr), Contiguous varr, ApplyUniversally v (Element varr), ApplyUniversally w (Element varr))
  => (forall a. k a -> v a -> Maybe (w a))
  -> Map karr varr k v
  -> Map karr varr k w
mapMaybeWithKey f (Map m) = id
  $ C.universally (Proxy :: Proxy k) (Proxy :: Proxy (Element karr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy v) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ C.applyUniversallyLifted (Proxy :: Proxy w) (Proxy :: Proxy (Element varr)) (Proxy :: Proxy Any)
  $ Map (M.mapMaybeWithKey (unsafeCoerceMapMaybeWithKeyFunction f) m)

