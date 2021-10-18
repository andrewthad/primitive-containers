{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Map.Interval.DBTS.Internal
  ( Map
  , pure
  , singleton
  , empty
  , lookup
  , union
  , unionWith
  , equals
  , map
  , mapBijection
  , traverseP
  , traverse
  , traverse_
  , fromList
  , foldrWithKey
  , foldlWithKeyM'
  , foldl'
  , foldlM'
  , foldMap
  , toList
  , showsPrec
  , concat
  , elems
  , size
  , convertKeys
  , convertKeysValues
  ) where

-- TODO: In very unusual situation where the keys or values
-- are passed to the FFI, the approach used here can lead to
-- unsoundness. This will be addressed in GHC 8.10.

import Prelude hiding (pure,lookup,compare,map,showsPrec,concat,traverse,foldMap)

import Control.Monad.ST (ST,runST)
import Control.Monad.Primitive (PrimMonad)
import Data.Kind (Type)
import Data.Primitive (PrimArray)
import Data.Primitive.Contiguous (Contiguous,Element,Mutable)
import GHC.Exts (ArrayArray#)
import qualified Data.Concatenation as C
import qualified Data.Primitive.Contiguous as I
import qualified Prelude as P

-- | The key array is the same length as the value array. Every key
--   is the upper bound of a range. The keys array always has a length
--   of at least one. The last element is always maxBound. The lowest bound
--   is assumed to be minBound. For example, the interval map of @Int16@:
--
--   > [-inf,5],[6,17],[18,20],[21,+inf]
--
--   Would be represented by the keys:
--   
--   > 5,17,20,65536
data Map :: (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type where
  MapInternal :: ArrayArray# -> ArrayArray# -> Map karr varr k v
  -- Map !(karr k) !(varr v)

typedArrays :: (Contiguous karr, Contiguous varr) => Map karr varr k v -> (karr k, varr v)
typedArrays (MapInternal ks vs) = (I.lift ks, I.lift vs)

typedValues :: Contiguous varr => Map karr varr k v -> (# ArrayArray#, varr v #)
typedValues (MapInternal ks vs) = (# ks, I.lift vs #)

typedKeys :: Contiguous karr => Map karr varr k v -> (# karr k, ArrayArray# #)
typedKeys (MapInternal ks vs) = (# I.lift ks, vs #)

pattern Map :: (Contiguous karr, Contiguous varr) => () => karr k -> varr v -> Map karr varr k v
pattern Map ks vs <- (typedArrays -> (ks,vs)) where
  Map xs ys = MapInternal (I.unlift xs) (I.unlift ys)

pattern MapValues :: Contiguous varr => () => ArrayArray# -> varr v -> Map karr varr k v
pattern MapValues ks vs <- (typedValues -> (# ks, vs #)) where
  MapValues xs ys = MapInternal xs (I.unlift ys)

pattern MapKeys :: Contiguous karr => () => karr k -> ArrayArray# -> Map karr varr k v
pattern MapKeys ks vs <- (typedKeys -> (# ks, vs #)) where
  MapKeys xs ys = MapInternal (I.unlift xs) ys

{-# COMPLETE Map #-}
{-# COMPLETE MapValues #-}
{-# COMPLETE MapKeys #-}

equals :: (Contiguous karr, Element karr k, Eq k, Contiguous varr, Element varr v, Eq v) => Map karr varr k v -> Map karr varr k v -> Bool
equals (Map k1 v1) (Map k2 v2) = I.equals k1 k2 && I.equals v1 v2

size :: (Contiguous varr, Element varr v)
  => Map karr varr k v
  -> Int
size (MapValues _ v) = I.size v

-- compare :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v, Ord v) => Map karr varr k v -> Map karr varr k v -> Bool
-- compare (Map k1 v1) (Map k2 v2) = mappend (I.compare k1 k2) (I.compare v1 v2)

-- Note: this is only correct when the function is a bijection.
mapBijection :: (Contiguous varr, Element varr v, Element varr w)
  => (v -> w) -> Map karr varr k v -> Map karr varr k w
mapBijection f (MapValues k v) = MapValues k (I.map f v)

-- The function does not need to be a bijection. It may cause adjacent
-- keys to collapse if their values become the same.
map :: forall karr varr k v w. (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Element varr w, Eq w)
  => (v -> w)
  -> Map karr varr k v
  -> Map karr varr k w
map f (Map keys vals) = runST action where
  !sz = I.size vals
  action :: forall s. ST s (Map karr varr k w)
  action = do
    m <- I.new sz
    let go :: Int -> Int -> w -> [Int] -> Int -> ST s (Int,[Int],Int)
        go !ixSrc !ixDst !prevVal !dropped !droppedCount = if ixSrc < sz
          then do
            oldVal <- I.indexM vals ixSrc
            let val = f oldVal
            if val == prevVal
              then go (ixSrc + 1) ixDst val ((ixSrc - 1) : dropped) (droppedCount + 1)
              else do
                I.write m ixDst val
                go (ixSrc + 1) (ixDst + 1) val dropped droppedCount
          else return (ixDst,dropped,droppedCount)
    v0 <- I.indexM vals 0
    let !w0 = f v0
    I.write m 0 w0
    (len,dropped,droppedCount) <- go 1 1 w0 [] 0
    vals' <- I.resize m len >>= I.unsafeFreeze
    case droppedCount of
      0 -> return (Map keys vals')
      _ -> do
        n <- I.new len
        let !(d :: PrimArray Int) = I.unsafeFromListReverseN (droppedCount + 1) (maxBound : dropped)
        let run :: Int -> Int -> Int -> ST s ()
            run !ixKey !ixDst !ixDrop = if ixKey < sz
              then if I.index d ixDrop == ixKey
                then run (ixKey + 1) ixDst (ixDrop + 1)
                else do
                  I.write n ixDst =<< I.indexM keys ixKey
                  run (ixKey + 1) (ixDst + 1) ixDrop
              else return ()
        run 0 0 0
        keys' <- I.unsafeFreeze n
        return (Map keys' vals')
        

-- Note: this is only correct when the function is a bijection.
traverseP :: (Contiguous varr, Element varr v, Element varr w, PrimMonad m)
  => (v -> m w) -> Map karr varr k v -> m (Map karr varr k w)
traverseP f (MapValues k v) = fmap (MapValues k) (I.traverseP f v)

-- Note: this is only correct when the function is a bijection.
traverse :: (Contiguous varr, Element varr v, Element varr w, Applicative m)
  => (v -> m w) -> Map karr varr k v -> m (Map karr varr k w)
traverse f (MapValues k v) = fmap (MapValues k) (I.traverse f v)

traverse_ :: (Contiguous varr, Element varr v, Applicative m)
  => (v -> m w) -> Map karr varr k v -> m ()
traverse_ f (MapValues _ v) = I.traverse_ f v

pure :: (Contiguous karr, Contiguous varr, Element karr k, Element varr v, Bounded k) => v -> Map karr varr k v
pure v = Map
  (runST $ do
     !(arr :: Mutable karr s k) <- I.replicateMut 1 maxBound
     I.unsafeFreeze arr
  )
  (runST $ do
     !(arr :: Mutable varr s v) <- I.replicateMut 1 v
     I.unsafeFreeze arr
  )

-- This is not actually empty, but it is the monoidal identity.
empty :: (Contiguous karr, Contiguous varr, Element karr k, Element varr v, Bounded k, Monoid v) => Map karr varr k v
empty = pure mempty

singleton :: forall karr varr k v. (Contiguous karr, Contiguous varr, Element karr k, Element varr v, Bounded k, Enum k, Ord k, Eq v)
  => v -- value outside of the interval
  -> k -- lower bound
  -> k -- upper bound
  -> v -- value inside the interval
  -> Map karr varr k v
singleton def lo hi v = if lo <= hi && def /= v
  then if lo > minBound
    then if hi < maxBound
      then Map
        (runST $ do
           !(arr :: Mutable karr s k) <- I.new 3
           I.write arr 0 (pred lo)
           I.write arr 1 hi
           I.write arr 2 maxBound
           I.unsafeFreeze arr
        )
        (runST $ do
           !(arr :: Mutable varr s v) <- I.new 3
           I.write arr 0 def
           I.write arr 1 v
           I.write arr 2 def
           I.unsafeFreeze arr
        )
      else Map
        (runST $ do
           !(arr :: Mutable karr s k) <- I.new 2
           I.write arr 0 (pred lo)
           I.write arr 1 maxBound
           I.unsafeFreeze arr
        )
        (runST $ do
           !(arr :: Mutable varr s v) <- I.new 2
           I.write arr 0 def
           I.write arr 1 v
           I.unsafeFreeze arr
        )
    else if hi < maxBound
      then Map
        (runST $ do
           !(arr :: Mutable karr s k) <- I.new 2
           I.write arr 0 hi
           I.write arr 1 maxBound
           I.unsafeFreeze arr
        )
        (runST $ do
           !(arr :: Mutable varr s v) <- I.new 2
           I.write arr 0 v
           I.write arr 1 def
           I.unsafeFreeze arr
        )
      else pure v
  else pure def

lookup :: forall karr varr k v. (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v)
  => k -> Map karr varr k v -> v
lookup a (Map keys vals) = go 0 (I.size vals - 1)
  where
  go :: Int -> Int -> v
  go !start !end
    -- The threshold used here could be any nonnegative number.
    -- This algorithm will be correct regardless. Switching from
    -- a divide-and-conquer approach to a simple scan when the map
    -- is small improves performance.
    | delta > 8 =
        let !mid = div (end + start) 2
            !valHi = I.index keys mid
         in case P.compare a valHi of
              LT -> go start mid
              EQ -> let !(# v #) = I.index# vals mid in v
              GT -> go (mid + 1) end
    | otherwise = finish start end
    where !delta = end - start
  finish :: Int -> Int -> v
  finish !start !end =
    let !(# val #) = I.index# keys start
     in if a > val
          then finish (start + 1) end
          else let !(# v #) = I.index# vals start in v
{-# INLINEABLE lookup #-}

union :: forall karr varr k v. (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v, Eq v, Semigroup v)
  => Map karr varr k v
  -> Map karr varr k v
  -> Map karr varr k v
union = unionWith (<>)

-- This is also known as liftA2
unionWith :: forall karr aarr barr carr k a b c. (Contiguous karr, Element karr k, Ord k, Contiguous aarr, Element aarr a, Contiguous barr, Element barr b, Contiguous carr, Element carr c, Eq c)
  => (a -> b -> c)
  -> Map karr aarr k a
  -> Map karr barr k b
  -> Map karr carr k c
unionWith combine (Map keysA valsA) (Map keysB valsB) = runST action where
  action :: forall s. ST s (Map karr carr k c)
  action = do
    let szA = I.size keysA
        szB = I.size keysB
        szMax = szA + szB
    keysDst <- I.new szMax
    valsDst <- I.new szMax
    -- For total maps, we don't have to worry about one map running out
    -- before the other. Also, this function has a precondition that
    -- all three indices are greater than zero.
    let go :: Int -> Int -> Int -> c -> ST s Int
        go !ixA !ixB !ixDst prevVal = if ixA < szA && ixB < szB
          then do
            keyA <- I.indexM keysA ixA
            keyB <- I.indexM keysB ixB
            case P.compare keyA keyB of
              EQ -> do
                valA <- I.indexM valsA ixA
                valB <- I.indexM valsB ixB
                let !v = combine valA valB
                if v == prevVal
                  then do
                    I.write keysDst (ixDst - 1) keyA
                    go (ixA + 1) (ixB + 1) ixDst v
                  else do
                    I.write keysDst ixDst keyA
                    I.write valsDst ixDst v
                    go (ixA + 1) (ixB + 1) (ixDst + 1) v
              LT -> do
                valA <- I.indexM valsA ixA
                valB <- I.indexM valsB ixB
                let !v = combine valA valB
                if v == prevVal
                  then do
                    I.write keysDst (ixDst - 1) keyA
                    go (ixA + 1) ixB ixDst v
                  else do
                    I.write keysDst ixDst keyA
                    I.write valsDst ixDst v
                    go (ixA + 1) ixB (ixDst + 1) v
              GT -> do
                valA <- I.indexM valsA ixA
                valB <- I.indexM valsB ixB
                let !v = combine valA valB
                if v == prevVal
                  then do
                    I.write keysDst (ixDst - 1) keyB
                    go ixA (ixB + 1) ixDst v
                  else do
                    I.write keysDst ixDst keyB
                    I.write valsDst ixDst v
                    go ixA (ixB + 1) (ixDst + 1) v
          else return ixDst
    keyA <- I.indexM keysA 0
    keyB <- I.indexM keysB 0
    valA <- I.indexM valsA 0
    valB <- I.indexM valsB 0
    let v = combine valA valB
    dstIx <- case P.compare keyA keyB of
      EQ -> do
        I.write keysDst 0 keyA
        I.write valsDst 0 v
        go 1 1 1 v
      LT -> do
        I.write keysDst 0 keyA
        I.write valsDst 0 v
        go 1 0 1 v
      GT -> do
        I.write keysDst 0 keyB
        I.write valsDst 0 v
        go 0 1 1 v
    keys <- I.resize keysDst dstIx >>= I.unsafeFreeze
    vals <- I.resize valsDst dstIx >>= I.unsafeFreeze
    return (Map keys vals)

showsPrec :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Bounded k, Enum k, Show k, Show v)
  => Int -> Map karr varr k v -> ShowS
showsPrec p m = showParen (p > 10)
  $ showString "fromList "
  . shows (toList m)

foldrWithKey :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Bounded k, Enum k)
  => (k -> k -> v -> b -> b)
  -> b
  -> Map karr varr k v
  -> b
foldrWithKey f z (Map keys vals) =
  let !sz = I.size vals
      -- we must be lazy in the second argument
      go !i lo
        | i == sz = z
        | otherwise =
            let !hi = I.index keys i
                !(# v #) = I.index# vals i
             in f lo hi v (go (i + 1) (succ hi))
   in go 0 minBound

foldlWithKeyM' :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Bounded k, Enum k, Monad m)
  => (b -> k -> k -> v -> m b)
  -> b
  -> Map karr varr k v
  -> m b
foldlWithKeyM' f z (Map keys vals) =
  let !sz = I.size vals
      -- we must be lazy in the third argument
      go !i !acc lo
        | i == sz = return acc
        | otherwise = do
            let !hi = I.index keys i
                !(# v #) = I.index# vals i
            acc' <- f acc lo hi v
            go (i + 1) acc' (succ hi)
   in go 0 z minBound

foldl' :: (Contiguous varr, Element varr v)
  => (b -> v -> b)
  -> b
  -> Map karr varr k v
  -> b
foldl' f b0 (MapValues _ vals) = I.foldl' f b0 vals

foldlM' :: (Contiguous varr, Element varr v, Monad m)
  => (b -> v -> m b)
  -> b
  -> Map karr varr k v
  -> m b
foldlM' f b0 (MapValues _ vals) = I.foldlM' f b0 vals

foldMap :: (Contiguous varr, Element varr v, Monoid m)
  => (v -> m)
  -> Map karr varr k v
  -> m
foldMap f (MapValues _ vals) = I.foldMap f vals

toList :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Bounded k, Enum k)
  => Map karr varr k v
  -> [(k,k,v)]
toList = foldrWithKey (\lo hi v xs -> (lo,hi,v) : xs) []

fromList :: (Contiguous karr, Element karr k, Bounded k, Ord k, Enum k, Contiguous varr, Element varr v, Eq v)
  => v -- value outside of the ranges
  -> [(k,k,v)]
  -> Map karr varr k v
fromList def xs = concatWith
  def
  (\x y -> if x == def then y else x)
  (P.map (\(lo,hi,v) -> singleton def lo hi v) xs)

concatWith :: forall karr varr k v. (Contiguous karr, Bounded k, Element karr k, Ord k, Contiguous varr, Element varr v, Eq v)
  => v -- value used if the list is empty
  -> (v -> v -> v)
  -> [Map karr varr k v]
  -> Map karr varr k v
concatWith def combine = C.concatSized size (pure def) (unionWith combine)

concat :: (Contiguous karr, Bounded k, Element karr k, Ord k, Contiguous varr, Element varr v, Eq v, Monoid v)
  => [Map karr varr k v]
  -> Map karr varr k v
concat = concatWith mempty mappend

elems :: Contiguous varr => Map karr varr k v -> varr v
elems (MapValues _ v) = v

-- TODO: use convert instead of map once that function
-- is released in a version of contiguous.
convertKeys :: (Contiguous karr, Element karr k, Contiguous jarr, Element jarr k)
  => Map karr varr k v -> Map jarr varr k v
convertKeys (MapKeys ks vs) = MapKeys (I.map id ks) vs

-- TODO: use convert instead of map once that function
-- is released in a version of contiguous.
convertKeysValues :: (Contiguous karr, Element karr k, Contiguous jarr, Element jarr k, Contiguous varr, Element varr v, Contiguous warr, Element warr v)
  => Map karr varr k v -> Map jarr warr k v
convertKeysValues (Map ks vs) = Map (I.map id ks) (I.map id vs)

