{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -O2 -Wall #-}
module Map
  ( Map
  , empty
  , singleton
  , map
  , append
  , lookup
  , showsPrec
  , equals
  , compare
  , toList
  , concat
  , size
    -- list conversion
  , fromListN
  , fromList
  , fromListAppend
  , fromListAppendN
  ) where

import Prelude hiding (compare,showsPrec,lookup,map,concat)

import Control.Applicative (liftA2)
import Control.Monad.ST (ST,runST)
import Data.Semigroup (Semigroup)
import Data.Foldable (foldl')
import qualified Data.List as L
import qualified Data.Semigroup as SG
import qualified Key as K
import qualified Prelude as P
import qualified Value as V

data Map k v = Map
  {-# UNPACK #-} !(K.Arr k)
  {-# UNPACK #-} !(V.Arr v)

empty :: (K.Ctx k, V.Ctx v) => Map k v
empty = Map
  (runST (K.new 0 >>= K.unsafeFreeze))
  (runST (V.new 0 >>= V.unsafeFreeze))

singleton :: (K.Ctx k, V.Ctx v) => k -> v -> Map k v
singleton k v = Map
  ( runST $ do
      arr <- K.new 1
      K.write arr 0 k
      K.unsafeFreeze arr
  )
  ( runST $ do
      arr <- V.new 1
      V.write arr 0 v
      V.unsafeFreeze arr
  )

equals :: (K.Ctx k, Eq k, V.Ctx v, Eq v) => Map k v -> Map k v -> Bool
equals (Map k1 v1) (Map k2 v2) = k1 == k2 && v1 == v2

compare :: (K.Ctx k, Ord k, V.Ctx v, Ord v) => Map k v -> Map k v -> Ordering
compare m1 m2 = P.compare (toList m1) (toList m2)

fromListWithN :: (K.Ctx k, Ord k, V.Ctx v) => (v -> v -> v) -> Int -> [(k,v)] -> Map k v
fromListWithN combine n xs =
  case xs of
    [] -> empty
    (k,v) : ys ->
      let (leftovers, result) = fromAscListWith combine (max 1 n) k v ys
       in concatWith combine (result : P.map (uncurry singleton) leftovers)

fromListN :: (K.Ctx k, Ord k, V.Ctx v) => Int -> [(k,v)] -> Map k v
fromListN = fromListWithN (\_ a -> a)

fromList :: (K.Ctx k, Ord k, V.Ctx v) => [(k,v)] -> Map k v
fromList = fromListN 1

fromListAppendN :: (K.Ctx k, Ord k, V.Ctx v, Semigroup v) => Int -> [(k,v)] -> Map k v
fromListAppendN = fromListWithN (SG.<>)

fromListAppend :: (K.Ctx k, Ord k, V.Ctx v, Semigroup v) => [(k,v)] -> Map k v
fromListAppend = fromListAppendN 1

fromAscListWith :: forall k v. (K.Ctx k, Ord k, V.Ctx v)
  => (v -> v -> v)
  -> Int -- initial size of buffer, must be 1 or higher
  -> k -- first key
  -> v -- first value
  -> [(k,v)] -- elements
  -> ([(k,v)], Map k v)
fromAscListWith combine !n !k0 !v0 xs0 = runST $ do
  keys0 <- K.new n
  vals0 <- V.new n
  K.write keys0 0 k0
  V.write vals0 0 v0
  let go :: forall s. Int -> k -> Int -> K.MArr s k -> V.MArr s v -> [(k,v)] -> ST s ([(k,v)], Map k v)
      go !ix !_ !sz !keys !vals [] = if ix == sz
        then do
          arrKeys <- K.unsafeFreeze keys
          arrVals <- V.unsafeFreeze vals
          return ([],Map arrKeys arrVals)
        else do
          keys' <- K.resize keys ix
          arrKeys <- K.unsafeFreeze keys'
          vals' <- V.resize vals ix
          arrVals <- V.unsafeFreeze vals'
          return ([],Map arrKeys arrVals)
      go !ix !old !sz !keys !vals ((k,v) : xs) = if ix < sz
        then case P.compare k old of
          GT -> do
            K.write keys ix k
            V.write vals ix v
            go (ix + 1) k sz keys vals xs
          EQ -> do
            !oldVal <- V.read vals (ix - 1)
            let !newVal = combine oldVal v
            V.write vals (ix - 1) newVal
            go ix k sz keys vals xs
          LT -> do
            keys' <- K.resize keys ix
            arrKeys <- K.unsafeFreeze keys'
            vals' <- V.resize vals ix
            arrVals <- V.unsafeFreeze vals'
            return ((k,v) : xs,Map arrKeys arrVals)
        else do
          let sz' = sz * 2
          keys' <- K.resize keys sz'
          vals' <- V.resize vals sz'
          go ix old sz' keys' vals' ((k,v) : xs)
  go 1 k0 n keys0 vals0 xs0


map :: (V.Ctx v, V.Ctx w) => (v -> w) -> Map k v -> Map k w
map f (Map k v) = Map k (V.map f v)

showsPrec :: (K.Ctx k, Show k, V.Ctx v, Show v) => Int -> Map k v -> ShowS
showsPrec p xs = showParen (p > 10) $
  showString "fromList " . shows (toList xs)

toList :: (K.Ctx k, V.Ctx v) => Map k v -> [(k,v)]
toList = foldrWithKey (\k v xs -> (k,v) : xs) []

foldrWithKey :: (K.Ctx k, V.Ctx v) => (k -> v -> b -> b) -> b -> Map k v -> b
foldrWithKey f z (Map keys vals) =
  let !sz = V.size vals
      go !i
        | i == sz = z
        | otherwise =
            let !k = K.index keys i
                !v = V.index vals i
             in f k v (go (i + 1))
   in go 0

concat :: (K.Ctx k, Ord k, V.Ctx v, Semigroup v) => [Map k v] -> Map k v
concat = concatWith (SG.<>)

concatWith :: forall k v. (K.Ctx k, Ord k, V.Ctx v) => (v -> v -> v) -> [Map k v] -> Map k v
concatWith combine = go [] where
  go :: [Map k v] -> [Map k v] -> Map k v
  go !stack [] = foldl' (appendWith combine) empty (L.reverse stack)
  go !stack (x : xs) = if size x > 0
    then go (pushStack x stack) xs
    else go stack xs
  pushStack :: Map k v -> [Map k v] -> [Map k v]
  pushStack x [] = [x]
  pushStack x (s : ss) = if size x >= size s
    then pushStack (appendWith combine s x) ss
    else x : s : ss

appendWith :: (K.Ctx k, V.Ctx v, Ord k) => (v -> v -> v) -> Map k v -> Map k v -> Map k v
appendWith combine (Map ksA vsA) (Map ksB vsB) =
  case unionArrWith combine ksA vsA ksB vsB of
    (k,v) -> Map k v
  
append :: (K.Ctx k, V.Ctx v, Ord k, Semigroup v) => Map k v -> Map k v -> Map k v
append (Map ksA vsA) (Map ksB vsB) =
  case unionArrWith (SG.<>) ksA vsA ksB vsB of
    (k,v) -> Map k v
  
unionArrWith :: (K.Ctx k, Ord k, V.Ctx v)
  => (v -> v -> v)
  -> K.Arr k -- keys a
  -> V.Arr v -- values a
  -> K.Arr k -- keys b
  -> V.Arr v -- values b
  -> (K.Arr k, V.Arr v)
unionArrWith combine keysA valsA keysB valsB
  | V.size valsA < 1 = (keysB,valsB)
  | V.size valsB < 1 = (keysA,valsA)
  | otherwise = runST $ do
      let !szA = V.size valsA
          !szB = V.size valsB
      !keysDst <- K.new (szA + szB)
      !valsDst <- V.new (szA + szB)
      let go !ixA !ixB !ixDst = if ixA < szA
            then if ixB < szB
              then do
                let !keyA = K.index keysA ixA
                    !keyB = K.index keysB ixB
                    !valA = V.index valsA ixA
                    !valB = V.index valsB ixB
                case P.compare keyA keyB of
                  EQ -> do
                    K.write keysDst ixDst keyA
                    let !r = combine valA valB
                    V.write valsDst ixDst r
                    go (ixA + 1) (ixB + 1) (ixDst + 1)
                  LT -> do
                    K.write keysDst ixDst keyA
                    V.write valsDst ixDst valA
                    go (ixA + 1) ixB (ixDst + 1)
                  GT -> do
                    K.write keysDst ixDst keyB
                    V.write valsDst ixDst valB
                    go ixA (ixB + 1) (ixDst + 1)
              else do
                K.copy keysDst ixDst keysA ixA (szA - ixA)
                V.copy valsDst ixDst valsA ixA (szA - ixA)
                return (ixDst + (szA - ixA))
            else if ixB < szB
              then do
                K.copy keysDst ixDst keysB ixB (szB - ixB)
                V.copy valsDst ixDst valsB ixB (szB - ixB)
                return (ixDst + (szB - ixB))
              else return ixDst
      !total <- go 0 0 0
      !keysFinal <- K.resize keysDst total
      !valsFinal <- V.resize valsDst total
      liftA2 (,) (K.unsafeFreeze keysFinal) (V.unsafeFreeze valsFinal)
 
lookup :: forall k v. (K.Ctx k, Ord k, V.Ctx v) => k -> Map k v -> Maybe v
lookup a (Map arr vals) = go 0 (V.size vals - 1) where
  go :: Int -> Int -> Maybe v
  go !start !end = if end < start
    then Nothing
    else
      let !mid = div (end + start) 2
          !v = K.index arr mid
       in case P.compare a v of
            LT -> go start (mid - 1)
            EQ -> Just (V.index vals mid)
            GT -> go (mid + 1) end
{-# INLINEABLE lookup #-}

size :: V.Ctx v => Map k v -> Int
size (Map _ arr) = V.size arr
