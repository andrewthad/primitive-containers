{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -O2 -Wall #-}
module Data.Map.Internal
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
import Data.Internal (Contiguous,Mutable,Element)
import qualified Data.List as L
import qualified Data.Semigroup as SG
import qualified Prelude as P
import qualified Data.Internal as I

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

fromListN :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v) => Int -> [(k,v)] -> Map karr varr k v
fromListN = fromListWithN (\_ a -> a)

fromList :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v) => [(k,v)] -> Map karr varr k v
fromList = fromListN 1

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
            !oldVal <- I.read vals (ix - 1)
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

showsPrec :: (Contiguous karr, Element karr k, Show k, Contiguous varr, Element varr v, Show v) => Int -> Map karr varr k v -> ShowS
showsPrec p xs = showParen (p > 10) $
  showString "fromList " . shows (toList xs)

toList :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v) => Map karr varr k v -> [(k,v)]
toList = foldrWithKey (\k v xs -> (k,v) : xs) []

foldrWithKey :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v) => (k -> v -> b -> b) -> b -> Map karr varr k v -> b
foldrWithKey f z (Map keys vals) =
  let !sz = I.size vals
      go !i
        | i == sz = z
        | otherwise =
            let !k = I.index keys i
                !v = I.index vals i
             in f k v (go (i + 1))
   in go 0

concat :: (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v, Semigroup v) => [Map karr varr k v] -> Map karr varr k v
concat = concatWith (SG.<>)

concatWith :: forall karr varr k v. (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v) => (v -> v -> v) -> [Map karr varr k v] -> Map karr varr k v
concatWith combine = go [] where
  go :: [Map karr varr k v] -> [Map karr varr k v] -> Map karr varr k v
  go !stack [] = foldl' (appendWith combine) empty (L.reverse stack)
  go !stack (x : xs) = if size x > 0
    then go (pushStack x stack) xs
    else go stack xs
  pushStack :: Map karr varr k v -> [Map karr varr k v] -> [Map karr varr k v]
  pushStack x [] = [x]
  pushStack x (s : ss) = if size x >= size s
    then pushStack (appendWith combine s x) ss
    else x : s : ss

appendWith :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Ord k) => (v -> v -> v) -> Map karr varr k v -> Map karr varr k v -> Map karr varr k v
appendWith combine (Map ksA vsA) (Map ksB vsB) =
  case unionArrWith combine ksA vsA ksB vsB of
    (k,v) -> Map k v
  
append :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Ord k, Semigroup v) => Map karr varr k v -> Map karr varr k v -> Map karr varr k v
append (Map ksA vsA) (Map ksB vsB) =
  case unionArrWith (SG.<>) ksA vsA ksB vsB of
    (k,v) -> Map k v
  
unionArrWith :: forall karr varr k v. (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v)
  => (v -> v -> v)
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
                    !valA = I.index valsA ixA
                    !valB = I.index valsB ixB
                case P.compare keyA keyB of
                  EQ -> do
                    I.write keysDst ixDst keyA
                    let !r = combine valA valB
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
 
lookup :: forall karr varr k v. (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v) => k -> Map karr varr k v -> Maybe v
lookup a (Map arr vals) = go 0 (I.size vals - 1) where
  go :: Int -> Int -> Maybe v
  go !start !end = if end < start
    then Nothing
    else
      let !mid = div (end + start) 2
          !v = I.index arr mid
       in case P.compare a v of
            LT -> go start (mid - 1)
            EQ -> Just (I.index vals mid)
            GT -> go (mid + 1) end
{-# INLINEABLE lookup #-}

size :: (Contiguous varr, Element varr v) => Map karr varr k v -> Int
size (Map _ arr) = I.size arr
