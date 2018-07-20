{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -O2 -Wall #-}
module Data.Map.Internal
  ( Map
  , empty
  , singleton
  , map
  , mapMaybe
  , mapMaybeWithKey
    -- * Folds
  , foldrWithKey
  , foldlWithKey'
  , foldrWithKey'
  , foldMapWithKey
  , foldMapWithKey'
    -- * Monadic Folds
  , foldlWithKeyM'
  , foldrWithKeyM'
  , foldlMapWithKeyM'
  , foldrMapWithKeyM'
    -- * Functions
  , append
  , appendWith
  , appendKeyWith
  , appendRightBiased
  , lookup
  , showsPrec
  , equals
  , compare
  , toList
  , concat
  , size
    -- * List Conversion
  , fromListN
  , fromList
  , fromListAppend
  , fromListAppendN
    -- * Array Conversion
  , unsafeFreezeZip
  ) where

import Prelude hiding (compare,showsPrec,lookup,map,concat)

import Control.Applicative (liftA2)
import Control.Monad.ST (ST,runST)
import Data.Semigroup (Semigroup)
import Data.Foldable (foldl')
import Data.Primitive.Contiguous (Contiguous,Mutable,Element)
import Data.Primitive.Sort (sortUniqueTaggedMutable)
import qualified Data.List as L
import qualified Data.Semigroup as SG
import qualified Prelude as P
import qualified Data.Primitive.Contiguous as I
import qualified Data.Concatenation as C

-- TODO: Do some sneakiness with UnliftedRep
data Map karr varr k v = Map !(karr k) !(varr v)

empty :: (Contiguous karr, Contiguous varr) => Map karr varr k v
empty = Map I.empty I.empty

singleton :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v) => k -> v -> Map karr varr k v
singleton k v = Map
  ( runST $ do
      arr <- I.new 1
      I.write arr 0 k
      I.unsafeFreeze arr
  )
  ( runST $ do
      arr <- I.new 1
      I.write arr 0 v
      I.unsafeFreeze arr
  )

equals :: (Contiguous karr, Element karr k, Eq k, Contiguous varr, Element varr v, Eq v) => Map karr varr k v -> Map karr varr k v -> Bool
equals (Map k1 v1) (Map k2 v2) = I.equals k1 k2 && I.equals v1 v2

compare :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v, Ord v) => Map karr varr k v -> Map karr varr k v -> Ordering
compare m1 m2 = P.compare (toList m1) (toList m2)

fromListWithN :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v) => (v -> v -> v) -> Int -> [(k,v)] -> Map karr varr k v
fromListWithN combine n xs =
  case xs of
    [] -> empty
    (k,v) : ys ->
      let (leftovers, result) = fromAscListWith combine (max 1 n) k v ys
       in concatWith combine (result : P.map (uncurry singleton) leftovers)

fromListN :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v)
  => Int
  -> [(k,v)]
  -> Map karr varr k v
{-# INLINABLE fromListN #-}
fromListN n xs = runST $ do
  (ks,vs) <- mutableArraysFromPairs (max n 1) xs
  unsafeFreezeZip ks vs

mutableArraysFromPairs :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v)
  => Int -- must be at least one
  -> [(k,v)]
  -> ST s (Mutable karr s k, Mutable varr s v)
{-# INLINABLE mutableArraysFromPairs #-}
mutableArraysFromPairs n xs = do
  let go !ix !_ !ks !vs [] = return (ix,ks,vs)
      go !ix !len !ks !vs ((k,v) : ys) = if ix < len
        then do
          I.write ks ix k
          I.write vs ix v
          go (ix + 1) len ks vs ys
        else do
          let len' = len * 2
          ks' <- I.new len'
          vs' <- I.new len'
          I.copyMutable ks' 0 ks 0 len
          I.copyMutable vs' 0 vs 0 len
          I.write ks' ix k
          I.write vs' ix v
          go (ix + 1) len' ks' vs' ys
  ks0 <- I.new n
  vs0 <- I.new n
  (len,ks',vs') <- go 0 n ks0 vs0 xs
  ksFinal <- I.resize ks' len
  vsFinal <- I.resize vs' len
  return (ksFinal,vsFinal)

fromList :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v) => [(k,v)] -> Map karr varr k v
fromList = fromListN 8

fromListAppendN :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v, Semigroup v) => Int -> [(k,v)] -> Map karr varr k v
fromListAppendN = fromListWithN (SG.<>)

fromListAppend :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v, Semigroup v) => [(k,v)] -> Map karr varr k v
fromListAppend = fromListAppendN 1

fromAscListWith :: forall karr varr k v. (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v)
  => (v -> v -> v)
  -> Int -- initial size of buffer, must be 1 or higher
  -> k -- first key
  -> v -- first value
  -> [(k,v)] -- elements
  -> ([(k,v)], Map karr varr k v)
fromAscListWith combine !n !k0 !v0 xs0 = runST $ do
  keys0 <- I.new n
  vals0 <- I.new n
  I.write keys0 0 k0
  I.write vals0 0 v0
  let go :: forall s. Int -> k -> Int -> Mutable karr s k -> Mutable varr s v -> [(k,v)] -> ST s ([(k,v)], Map karr varr k v)
      go !ix !_ !sz !keys !vals [] = if ix == sz
        then do
          arrKeys <- I.unsafeFreeze keys
          arrVals <- I.unsafeFreeze vals
          return ([],Map arrKeys arrVals)
        else do
          keys' <- I.resize keys ix
          arrKeys <- I.unsafeFreeze keys'
          vals' <- I.resize vals ix
          arrVals <- I.unsafeFreeze vals'
          return ([],Map arrKeys arrVals)
      go !ix !old !sz !keys !vals ((k,v) : xs) = if ix < sz
        then case P.compare k old of
          GT -> do
            I.write keys ix k
            I.write vals ix v
            go (ix + 1) k sz keys vals xs
          EQ -> do
            oldVal <- I.read vals (ix - 1)
            let !newVal = combine oldVal v
            I.write vals (ix - 1) newVal
            go ix k sz keys vals xs
          LT -> do
            keys' <- I.resize keys ix
            arrKeys <- I.unsafeFreeze keys'
            vals' <- I.resize vals ix
            arrVals <- I.unsafeFreeze vals'
            return ((k,v) : xs,Map arrKeys arrVals)
        else do
          let sz' = sz * 2
          keys' <- I.resize keys sz'
          vals' <- I.resize vals sz'
          go ix old sz' keys' vals' ((k,v) : xs)
  go 1 k0 n keys0 vals0 xs0


map :: (Contiguous varr, Element varr v, Element varr w) => (v -> w) -> Map karr varr k v -> Map karr varr k w
map f (Map k v) = Map k (I.map f v)

-- | /O(n)/ Drop elements for which the predicate returns 'Nothing'.
mapMaybe :: forall karr varr k v w. (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Element varr w)
  => (v -> Maybe w)
  -> Map karr varr k v
  -> Map karr varr k w
{-# INLINEABLE mapMaybe #-}
mapMaybe f (Map ks vs) = runST $ do
  let !sz = I.size vs
  !(karr :: Mutable karr s k) <- I.new sz
  !(varr :: Mutable varr s w) <- I.new sz
  let go !ixSrc !ixDst = if ixSrc < sz
        then do
          a <- I.indexM vs ixSrc
          case f a of
            Nothing -> go (ixSrc + 1) ixDst
            Just b -> do
              I.write varr ixDst b
              I.write karr ixDst =<< I.indexM ks ixSrc
              go (ixSrc + 1) (ixDst + 1)
        else return ixDst
  dstLen <- go 0 0
  ksFinal <- I.resize karr dstLen >>= I.unsafeFreeze
  vsFinal <- I.resize varr dstLen >>= I.unsafeFreeze
  return (Map ksFinal vsFinal)

-- | /O(n)/ Drop elements for which the predicate returns 'Nothing'.
mapMaybeWithKey :: forall karr varr k v w. (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Element varr w)
  => (k -> v -> Maybe w)
  -> Map karr varr k v
  -> Map karr varr k w
{-# INLINEABLE mapMaybeWithKey #-}
mapMaybeWithKey f (Map ks vs) = runST $ do
  let !sz = I.size vs
  !(karr :: Mutable karr s k) <- I.new sz
  !(varr :: Mutable varr s w) <- I.new sz
  let go !ixSrc !ixDst = if ixSrc < sz
        then do
          k <- I.indexM ks ixSrc
          a <- I.indexM vs ixSrc
          case f k a of
            Nothing -> go (ixSrc + 1) ixDst
            Just !b -> do
              I.write varr ixDst b
              I.write karr ixDst k
              go (ixSrc + 1) (ixDst + 1)
        else return ixDst
  dstLen <- go 0 0
  ksFinal <- I.resize karr dstLen >>= I.unsafeFreeze
  vsFinal <- I.resize varr dstLen >>= I.unsafeFreeze
  return (Map ksFinal vsFinal)

showsPrec :: (Contiguous karr, Element karr k, Show k, Contiguous varr, Element varr v, Show v) => Int -> Map karr varr k v -> ShowS
showsPrec p xs = showParen (p > 10) $
  showString "fromList " . shows (toList xs)

toList :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v) => Map karr varr k v -> [(k,v)]
toList = foldrWithKey (\k v xs -> (k,v) : xs) []

foldrWithKey :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v)
  => (k -> v -> b -> b)
  -> b
  -> Map karr varr k v
  -> b
foldrWithKey f z (Map keys vals) =
  let !sz = I.size vals
      go !i
        | i == sz = z
        | otherwise =
            let !(# k #) = I.index# keys i
                !(# v #) = I.index# vals i
             in f k v (go (i + 1))
   in go 0

foldMapWithKey :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Monoid m)
  => (k -> v -> m)
  -> Map karr varr k v
  -> m
foldMapWithKey f (Map keys vals) =
  let !sz = I.size vals
      go !i
        | i == sz = mempty
        | otherwise =
            let !(# k #) = I.index# keys i
                !(# v #) = I.index# vals i
             in mappend (f k v) (go (i + 1))
   in go 0

concat :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v, Semigroup v) => [Map karr varr k v] -> Map karr varr k v
concat = concatWith (SG.<>)

concatWith :: forall karr varr k v. (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v)
  => (v -> v -> v)
  -> [Map karr varr k v]
  -> Map karr varr k v
concatWith combine = C.concatSized size empty (appendWith combine)

appendRightBiased :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Ord k) => Map karr varr k v -> Map karr varr k v -> Map karr varr k v
appendRightBiased = appendWith const

appendKeyWith :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Ord k)
  => (k -> v -> v -> v) -> Map karr varr k v -> Map karr varr k v -> Map karr varr k v
appendKeyWith combine (Map ksA vsA) (Map ksB vsB) =
  case unionArrWith combine ksA vsA ksB vsB of
    (k,v) -> Map k v
  
appendWith :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Ord k)
  => (v -> v -> v) -> Map karr varr k v -> Map karr varr k v -> Map karr varr k v
appendWith combine (Map ksA vsA) (Map ksB vsB) =
  case unionArrWith (\_ x y -> combine x y) ksA vsA ksB vsB of
    (k,v) -> Map k v
  
append :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Ord k, Semigroup v) => Map karr varr k v -> Map karr varr k v -> Map karr varr k v
append (Map ksA vsA) (Map ksB vsB) =
  case unionArrWith (\_ x y -> x SG.<> y) ksA vsA ksB vsB of
    (k,v) -> Map k v
  
unionArrWith :: forall karr varr k v. (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v)
  => (k -> v -> v -> v)
  -> karr k -- keys a
  -> varr v -- values a
  -> karr k -- keys b
  -> varr v -- values b
  -> (karr k, varr v)
unionArrWith combine keysA valsA keysB valsB
  | I.size valsA < 1 = (keysB,valsB)
  | I.size valsB < 1 = (keysA,valsA)
  | otherwise = runST $ do
      let !szA = I.size valsA
          !szB = I.size valsB
      !(keysDst :: Mutable karr s k) <- I.new (szA + szB)
      !(valsDst :: Mutable varr s v) <- I.new (szA + szB)
      let go !ixA !ixB !ixDst = if ixA < szA
            then if ixB < szB
              then do
                let !keyA = I.index keysA ixA
                    !keyB = I.index keysB ixB
                    !(# valA #) = I.index# valsA ixA
                    !(# valB #) = I.index# valsB ixB
                case P.compare keyA keyB of
                  EQ -> do
                    I.write keysDst ixDst keyA
                    let r = combine keyA valA valB
                    I.write valsDst ixDst r
                    go (ixA + 1) (ixB + 1) (ixDst + 1)
                  LT -> do
                    I.write keysDst ixDst keyA
                    I.write valsDst ixDst valA
                    go (ixA + 1) ixB (ixDst + 1)
                  GT -> do
                    I.write keysDst ixDst keyB
                    I.write valsDst ixDst valB
                    go ixA (ixB + 1) (ixDst + 1)
              else do
                I.copy keysDst ixDst keysA ixA (szA - ixA)
                I.copy valsDst ixDst valsA ixA (szA - ixA)
                return (ixDst + (szA - ixA))
            else if ixB < szB
              then do
                I.copy keysDst ixDst keysB ixB (szB - ixB)
                I.copy valsDst ixDst valsB ixB (szB - ixB)
                return (ixDst + (szB - ixB))
              else return ixDst
      !total <- go 0 0 0
      !keysFinal <- I.resize keysDst total
      !valsFinal <- I.resize valsDst total
      liftA2 (,) (I.unsafeFreeze keysFinal) (I.unsafeFreeze valsFinal)
 
lookup :: forall karr varr k v.
     (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v)
  => k
  -> Map karr varr k v
  -> Maybe v
lookup a (Map arr vals) = go 0 (I.size vals - 1) where
  go :: Int -> Int -> Maybe v
  go !start !end = if end < start
    then Nothing
    else
      let !mid = div (end + start) 2
          !(# v #) = I.index# arr mid
       in case P.compare a v of
            LT -> go start (mid - 1)
            EQ -> case I.index# vals mid of
              (# r #) -> Just r
            GT -> go (mid + 1) end
{-# INLINEABLE lookup #-}

size :: (Contiguous varr, Element varr v) => Map karr varr k v -> Int
size (Map _ arr) = I.size arr

-- | Sort and deduplicate the key array, preserving the last value associated
-- with each key. The argument arrays may not be reused after being passed
-- to this function.
unsafeFreezeZip :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v)
  => Mutable karr s k
  -> Mutable varr s v
  -> ST s (Map karr varr k v)
unsafeFreezeZip keys0 vals0 = do
  (keys1,vals1) <- sortUniqueTaggedMutable keys0 vals0
  keys2 <- I.unsafeFreeze keys1
  vals2 <- I.unsafeFreeze vals1
  return (Map keys2 vals2)
{-# INLINEABLE unsafeFreezeZip #-}

foldlWithKeyM' :: forall karr varr k v m b. (Monad m, Contiguous karr, Element karr k, Contiguous varr, Element varr v)
  => (b -> k -> v -> m b)
  -> b
  -> Map karr varr k v
  -> m b
foldlWithKeyM' f b0 (Map ks vs) = go 0 b0
  where
  !len = I.size vs
  go :: Int -> b -> m b
  go !ix !acc = if ix < len
    then
      let (# k #) = I.index# ks ix
          (# v #) = I.index# vs ix
       in f acc k v >>= go (ix + 1)
    else return acc
{-# INLINEABLE foldlWithKeyM' #-}

foldrWithKeyM' :: forall karr varr k v m b. (Monad m, Contiguous karr, Element karr k, Contiguous varr, Element varr v)
  => (k -> v -> b -> m b)
  -> b
  -> Map karr varr k v
  -> m b
foldrWithKeyM' f b0 (Map ks vs) = go (I.size vs - 1) b0
  where
  go :: Int -> b -> m b
  go !ix !acc = if ix >= 0
    then
      let (# k #) = I.index# ks ix
          (# v #) = I.index# vs ix
       in f k v acc >>= go (ix - 1)
    else return acc
{-# INLINEABLE foldrWithKeyM' #-}

foldlMapWithKeyM' :: forall karr varr k v m b. (Monad m, Contiguous karr, Element karr k, Contiguous varr, Element varr v, Monoid b)
  => (k -> v -> m b)
  -> Map karr varr k v
  -> m b
foldlMapWithKeyM' f (Map ks vs) = go 0 mempty
  where
  !len = I.size vs
  go :: Int -> b -> m b
  go !ix !accl = if ix < len
    then
      let (# k #) = I.index# ks ix
          (# v #) = I.index# vs ix
       in do
         accr <- f k v
         go (ix + 1) (mappend accl accr)
    else return accl
{-# INLINEABLE foldlMapWithKeyM' #-}

foldrMapWithKeyM' :: forall karr varr k v m b. (Monad m, Contiguous karr, Element karr k, Contiguous varr, Element varr v, Monoid b)
  => (k -> v -> m b)
  -> Map karr varr k v
  -> m b
foldrMapWithKeyM' f (Map ks vs) = go (I.size vs - 1) mempty
  where
  go :: Int -> b -> m b
  go !ix !accr = if ix >= 0
    then
      let (# k #) = I.index# ks ix
          (# v #) = I.index# vs ix
       in do
         accl <- f k v
         go (ix + 1) (mappend accl accr)
    else return accr
{-# INLINEABLE foldrMapWithKeyM' #-}

foldMapWithKey' :: forall karr varr k v m. (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Monoid m)
  => (k -> v -> m)
  -> Map karr varr k v
  -> m
foldMapWithKey' f (Map ks vs) = go 0 mempty
  where
  !len = I.size vs
  go :: Int -> m -> m
  go !ix !accl = if ix < len
    then 
      let (# k #) = I.index# ks ix
          (# v #) = I.index# vs ix
       in go (ix + 1) (mappend accl (f k v))
    else accl
{-# INLINEABLE foldMapWithKey' #-}

foldlWithKey' :: forall karr varr k v b. (Contiguous karr, Element karr k, Contiguous varr, Element varr v)
  => (b -> k -> v -> b) 
  -> b
  -> Map karr varr k v
  -> b
foldlWithKey' f b0 (Map ks vs) = go 0 b0
  where
  !len = I.size vs
  go :: Int -> b -> b
  go !ix !acc = if ix < len
    then 
      let (# k #) = I.index# ks ix
          (# v #) = I.index# vs ix
       in go (ix + 1) (f acc k v)
    else acc
{-# INLINEABLE foldlWithKey' #-}

foldrWithKey' :: forall karr varr k v b. (Contiguous karr, Element karr k, Contiguous varr, Element varr v)
  => (k -> v -> b -> b)
  -> b
  -> Map karr varr k v
  -> b
foldrWithKey' f b0 (Map ks vs) = go (I.size vs - 1) b0
  where
  go :: Int -> b -> b
  go !ix !acc = if ix >= 0
    then
      let (# k #) = I.index# ks ix
          (# v #) = I.index# vs ix
       in go (ix - 1) (f k v acc)
    else acc
{-# INLINEABLE foldrWithKey' #-}

