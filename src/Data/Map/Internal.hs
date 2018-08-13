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
  , null
  , map
  , mapWithKey
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
    -- * Traversals
  , traverseWithKey_
    -- * Functions
  , append
  , appendWith
  , appendKeyWith
  , appendRightBiased
  , intersectionWith
  , lookup
  , showsPrec
  , equals
  , compare
  , toList
  , concat
  , size
  , keys
  , elems
  , restrict
    -- * List Conversion
  , fromListN
  , fromList
  , fromListAppend
  , fromListAppendN
  , fromSet
    -- * Array Conversion
  , unsafeFreezeZip
  , unsafeZipPresorted
  ) where

import Prelude hiding (compare,showsPrec,lookup,map,concat,null)

import Control.Applicative (liftA2)
import Control.Monad.ST (ST,runST)
import Data.Semigroup (Semigroup)
import Data.Primitive.Contiguous (Contiguous,Mutable,Element)
import Data.Primitive.Sort (sortUniqueTaggedMutable)
import Data.Set.Internal (Set(..))
import qualified Data.List as L
import qualified Data.Semigroup as SG
import qualified Prelude as P
import qualified Data.Primitive.Contiguous as I
import qualified Data.Concatenation as C

-- TODO: Do some sneakiness with UnliftedRep
data Map karr varr k v = Map !(karr k) !(varr v)

empty :: (Contiguous karr, Contiguous varr) => Map karr varr k v
empty = Map I.empty I.empty

null :: Contiguous varr => Map karr varr k v -> Bool
null (Map _ vals) = I.null vals

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
      go !ix !_ !sz !theKeys !vals [] = if ix == sz
        then do
          arrKeys <- I.unsafeFreeze theKeys
          arrVals <- I.unsafeFreeze vals
          return ([],Map arrKeys arrVals)
        else do
          keys' <- I.resize theKeys ix
          arrKeys <- I.unsafeFreeze keys'
          vals' <- I.resize vals ix
          arrVals <- I.unsafeFreeze vals'
          return ([],Map arrKeys arrVals)
      go !ix !old !sz !theKeys !vals ((k,v) : xs) = if ix < sz
        then case P.compare k old of
          GT -> do
            I.write theKeys ix k
            I.write vals ix v
            go (ix + 1) k sz theKeys vals xs
          EQ -> do
            oldVal <- I.read vals (ix - 1)
            let !newVal = combine oldVal v
            I.write vals (ix - 1) newVal
            go ix k sz theKeys vals xs
          LT -> do
            keys' <- I.resize theKeys ix
            arrKeys <- I.unsafeFreeze keys'
            vals' <- I.resize vals ix
            arrVals <- I.unsafeFreeze vals'
            return ((k,v) : xs,Map arrKeys arrVals)
        else do
          let sz' = sz * 2
          keys' <- I.resize theKeys sz'
          vals' <- I.resize vals sz'
          go ix old sz' keys' vals' ((k,v) : xs)
  go 1 k0 n keys0 vals0 xs0


map :: (Contiguous varr, Element varr v, Element varr w) => (v -> w) -> Map karr varr k v -> Map karr varr k w
map f (Map k v) = Map k (I.map f v)

-- | /O(n)/ Drop elements for which the predicate returns 'Nothing'.
mapWithKey :: forall karr varr k v w. (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Element varr w)
  => (k -> v -> w)
  -> Map karr varr k v
  -> Map karr varr k w
{-# INLINEABLE mapWithKey #-}
mapWithKey f (Map ks vs) = runST $ do
  let !sz = I.size vs
  !(karr :: Mutable karr s k) <- I.new sz
  !(varr :: Mutable varr s w) <- I.new sz
  let go !ix = if ix < sz
        then do
          k <- I.indexM ks ix
          a <- I.indexM vs ix
          I.write varr ix (f k a)
          I.write karr ix k
          go (ix + 1)
        else return ix
  dstLen <- go 0
  ksFinal <- I.resize karr dstLen >>= I.unsafeFreeze
  vsFinal <- I.resize varr dstLen >>= I.unsafeFreeze
  return (Map ksFinal vsFinal)

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
foldrWithKey f z (Map theKeys vals) =
  let !sz = I.size vals
      go !i
        | i == sz = z
        | otherwise =
            let !(# k #) = I.index# theKeys i
                !(# v #) = I.index# vals i
             in f k v (go (i + 1))
   in go 0

foldMapWithKey :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Monoid m)
  => (k -> v -> m)
  -> Map karr varr k v
  -> m
foldMapWithKey f (Map theKeys vals) =
  let !sz = I.size vals
      go !i
        | i == sz = mempty
        | otherwise =
            let !(# k #) = I.index# theKeys i
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
  
append :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Ord k, Semigroup v)
  => Map karr varr k v -> Map karr varr k v -> Map karr varr k v
append (Map ksA vsA) (Map ksB vsB) =
  case unionArrWith (\_ x y -> x SG.<> y) ksA vsA ksB vsB of
    (k,v) -> Map k v
  
intersectionWith :: forall k v w x karr varr warr xarr.
     (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Contiguous warr, Element warr w, Contiguous xarr, Element xarr x, Ord k)
  => (v -> w -> x)
  -> Map karr varr k v
  -> Map karr warr k w
  -> Map karr xarr k x
intersectionWith f s1@(Map karr1 varr1) s2@(Map karr2 varr2)
  | sz1 == 0 = empty
  | sz2 == 0 = empty
  | otherwise = runST $ do
      let maxSz = min sz1 sz2
      kdst <- I.new maxSz
      vdst <- I.new maxSz
      let go !ix1 !ix2 !dstIx = if ix2 < sz2 && ix1 < sz1
            then do
              k1 <- I.indexM karr1 ix1
              k2 <- I.indexM karr2 ix2
              case P.compare k1 k2 of
                EQ -> do
                  v1 <- I.indexM varr1 ix1
                  v2 <- I.indexM varr2 ix2
                  I.write kdst dstIx k1
                  I.write vdst dstIx (f v1 v2)
                  go (ix1 + 1) (ix2 + 1) (dstIx + 1)
                LT -> go (ix1 + 1) ix2 dstIx
                GT -> go ix1 (ix2 + 1) dstIx
            else return dstIx
      dstSz <- go 0 0 0
      kdstFrozen <- I.resize kdst dstSz >>= I.unsafeFreeze
      vdstFrozen <- I.resize vdst dstSz >>= I.unsafeFreeze
      return (Map kdstFrozen vdstFrozen)
  where
    !sz1 = size s1
    !sz2 = size s2

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
-- to this function. This function is only unsafe because of the requirement
-- that the arguments not be reused. If the arrays do not match in size, the
-- larger one will be truncated to the length of the shorter one.
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

-- | There are two preconditions:
--
-- * The array of keys is sorted
-- * The array of keys and the array of values have the same length.
--
-- If either of these conditions is not met, this function will introduce
-- undefined behavior or segfaults.
unsafeZipPresorted :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v)
  => karr k -- array of keys, must already be sorted
  -> varr v -- array of values
  -> Map karr varr k v
unsafeZipPresorted = Map

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
      let !(# k #) = I.index# ks ix
          !(# v #) = I.index# vs ix
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
      let !(# k #) = I.index# ks ix
          !(# v #) = I.index# vs ix
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
      let !(# k #) = I.index# ks ix
          !(# v #) = I.index# vs ix
       in do
         accr <- f k v
         go (ix + 1) (mappend accl accr)
    else return accl
{-# INLINEABLE foldlMapWithKeyM' #-}

traverseWithKey_ :: forall karr varr k v m b. (Applicative m, Contiguous karr, Element karr k, Contiguous varr, Element varr v)
  => (k -> v -> m b)
  -> Map karr varr k v
  -> m ()
traverseWithKey_ f (Map ks vs) = go 0
  where
  !len = I.size vs
  go :: Int -> m ()
  go !ix = if ix < len
    then
      let !(# k #) = I.index# ks ix
          !(# v #) = I.index# vs ix
       in f k v *> go (ix + 1)
    else pure ()
{-# INLINEABLE traverseWithKey_ #-}

foldrMapWithKeyM' :: forall karr varr k v m b. (Monad m, Contiguous karr, Element karr k, Contiguous varr, Element varr v, Monoid b)
  => (k -> v -> m b)
  -> Map karr varr k v
  -> m b
foldrMapWithKeyM' f (Map ks vs) = go (I.size vs - 1) mempty
  where
  go :: Int -> b -> m b
  go !ix !accr = if ix >= 0
    then
      let !(# k #) = I.index# ks ix
          !(# v #) = I.index# vs ix
       in do
         accl <- f k v
         go (ix - 1) (mappend accl accr)
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
      let !(# k #) = I.index# ks ix
          !(# v #) = I.index# vs ix
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
      let !(# k #) = I.index# ks ix
          !(# v #) = I.index# vs ix
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
      let !(# k #) = I.index# ks ix
          !(# v #) = I.index# vs ix
       in go (ix - 1) (f k v acc)
    else acc
{-# INLINEABLE foldrWithKey' #-}

-- The algorithm used here is good when the subset is small, but
-- when the subset is large, it is worse that just walking the map.
restrict :: forall karr varr k v. (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Ord k)
  => Map karr varr k v
  -> Set karr k
  -> Map karr varr k v
restrict m@(Map ks vs) (Set rs)
  | I.same ks rs = m
  | otherwise = stage1 0
  where
  szMap = I.size vs
  szSet = I.size rs
  szMin = min szMap szSet
  -- Locate the first difference between the two. This stage is useful
  -- because, in the case that the subset perfectly matches the keys,
  -- we do not need to do any copying.
  stage1 :: Int -> Map karr varr k v
  stage1 !ix = if ix < szMin
    then
      let !(# k #) = I.index# ks ix
          !(# r #) = I.index# rs ix
       in if k == r
            then stage1 (ix + 1)
            else stage2 ix
    else if szMin == szMap
      then m
      else Map rs vs
  -- In stage two, we walk the map and the set with possibly differing
  -- indices, writing each matching key (along with its value) into
  -- the result map.
  stage2 :: Int -> Map karr varr k v
  stage2 !ix = runST $ do
    ksMut <- I.new szMin
    vsMut <- I.new szMin
    I.copy ksMut 0 ks 0 ix
    I.copy vsMut 0 vs 0 ix
    let -- TODO: Turn this into a galloping search. It would
        -- probably be worth trying this out on
        -- Data.Set.Internal.intersection first.
        go !ixRes !ixm !ixs = if ixm < szMin && ixs < szMin
          then do
            k <- I.indexM ks ixm
            r <- I.indexM rs ixs
            case P.compare k r of
              EQ -> do
                I.write ksMut ixRes k
                I.write vsMut ixRes =<< I.indexM vs ixm
                go (ixRes + 1) (ixm + 1) (ixs + 1)
              LT -> go ixRes (ixm + 1) ixs
              GT -> go ixRes ixm (ixs + 1)
          else return ixRes
    total <- go ix ix ix
    ks' <- I.resize ksMut total >>= I.unsafeFreeze
    vs' <- I.resize vsMut total >>= I.unsafeFreeze
    return (Map ks' vs')
{-# INLINEABLE restrict #-}

fromSet :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v)
  => (k -> v)
  -> Set karr k
  -> Map karr varr k v
fromSet f (Set arr) = Map arr (I.map f arr)

keys :: Map karr varr k v -> Set karr k
keys (Map k _) = Set k

elems :: Map karr varr k v -> varr v
elems (Map _ v) = v

