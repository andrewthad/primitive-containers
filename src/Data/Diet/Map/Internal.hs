{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -O2 -Wall #-}
module Data.Diet.Map.Internal
  ( Map
  , empty
  , singleton
  , map
  , append
  , lookup
  , concat
  , equals
  , showsPrec
  , liftShowsPrec2
    -- list conversion
  , fromListN
  , fromList
  , fromListAppend
  , fromListAppendN
  , toList
  ) where

import Prelude hiding (lookup,showsPrec,concat,map)

import Control.Applicative (liftA2)
import Control.Monad.ST (ST,runST)
import Data.Semigroup (Semigroup)
import Data.Foldable (foldl')
import Text.Show (showListWith)
import Data.Primitive.Contiguous (Contiguous,Element,Mutable)
import qualified Data.List as L
import qualified Data.Semigroup as SG
import qualified Prelude as P
import qualified Data.Primitive.Contiguous as I
import qualified Data.Concatenation as C

-- The key array is twice as long as the value array since
-- everything is stored as a range. Also, figure out how to
-- unpack these two arguments at some point.
data Map karr varr k v = Map !(karr k) !(varr v)

empty :: (Contiguous karr, Contiguous varr) => Map karr varr k v
empty = Map I.empty I.empty

map :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v, Element varr w) => (v -> w) -> Map karr varr k v -> Map karr varr k w
map f (Map k v) = Map k (I.map f v)

equals :: (Contiguous karr, Element karr k, Eq k, Contiguous varr, Element varr v, Eq v) => Map karr varr k v -> Map karr varr k v -> Bool
equals (Map k1 v1) (Map k2 v2) = I.equals k1 k2 && I.equals v1 v2

fromListN :: (Contiguous karr, Element karr k, Ord k, Enum k, Contiguous varr, Element varr v, Eq v) => Int -> [(k,k,v)] -> Map karr varr k v
fromListN = fromListWithN (\_ a -> a)

fromList :: (Contiguous karr, Element karr k, Ord k, Enum k, Contiguous varr, Element varr v, Eq v) => [(k,k,v)] -> Map karr varr k v
fromList = fromListN 1

fromListAppendN :: (Contiguous karr, Element karr k, Ord k, Enum k, Contiguous varr, Element varr v, Semigroup v, Eq v) => Int -> [(k,k,v)] -> Map karr varr k v
fromListAppendN = fromListWithN (SG.<>)

fromListAppend :: (Contiguous karr, Element karr k, Ord k, Enum k, Contiguous varr, Element varr v, Semigroup v, Eq v) => [(k,k,v)] -> Map karr varr k v
fromListAppend = fromListAppendN 1

fromListWithN :: (Contiguous karr, Element karr k, Ord k, Enum k, Contiguous varr, Element varr v, Eq v) => (v -> v -> v) -> Int -> [(k,k,v)] -> Map karr varr k v
fromListWithN combine _ xs =
  concatWith combine (P.map (\(lo,hi,v) -> singleton lo hi v) xs)

concat :: (Contiguous karr, Element karr k, Ord k, Enum k, Contiguous varr, Element varr v, Semigroup v, Eq v) => [Map karr varr k v] -> Map karr varr k v
concat = concatWith (SG.<>)

singleton :: forall karr varr k v. (Contiguous karr, Element karr k,Ord k,Contiguous varr, Element varr v) => k -> k -> v -> Map karr varr k v
singleton !lo !hi !v = if lo <= hi
  then Map
    ( runST $ do
        !(arr :: Mutable karr s k) <- I.new 2
        I.write arr 0 lo
        I.write arr 1 hi
        I.unsafeFreeze arr
    )
    ( runST $ do
        !(arr :: Mutable varr s v) <- I.new 1
        I.write arr 0 v
        I.unsafeFreeze arr
    )
  else empty

lookup :: forall karr varr k v. (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v) => k -> Map karr varr k v -> Maybe v
lookup a (Map keys vals) = go 0 (I.size vals - 1) where
  go :: Int -> Int -> Maybe v
  go !start !end = if end <= start
    then if end == start
      then 
        let !valLo = I.index keys (2 * start)
            !valHi = I.index keys (2 * start + 1)
         in if a >= valLo && a <= valHi
              then case I.index# vals start of
                (# v #) -> Just v
              else Nothing
      else Nothing
    else
      let !mid = div (end + start + 1) 2
          !valLo = I.index keys (2 * mid)
       in case P.compare a valLo of
            LT -> go start (mid - 1)
            EQ -> case I.index# vals mid of
              (# v #) -> Just v
            GT -> go mid end
{-# INLINEABLE lookup #-}


append :: (Contiguous karr, Element karr k, Ord k, Enum k, Contiguous varr, Element varr v, Semigroup v, Eq v) => Map karr varr k v -> Map karr varr k v -> Map karr varr k v
append (Map ksA vsA) (Map ksB vsB) =
  case unionArrWith (SG.<>) ksA vsA ksB vsB of
    (k,v) -> Map k v

appendWith :: (Contiguous karr, Element karr k, Ord k, Enum k, Contiguous varr, Element varr v, Eq v) => (v -> v -> v) -> Map karr varr k v -> Map karr varr k v -> Map karr varr k v
appendWith combine (Map ksA vsA) (Map ksB vsB) =
  case unionArrWith combine ksA vsA ksB vsB of
    (k,v) -> Map k v
  
  
unionArrWith :: forall karr varr k v. (Contiguous karr, Element karr k, Ord k, Enum k, Contiguous varr, Element varr v, Eq v)
  => (v -> v -> v)
  -> karr k -- keys a
  -> varr v -- values a
  -> karr k -- keys b
  -> varr v -- values b
  -> (karr k, varr v)
unionArrWith combine keysA valsA keysB valsB
  | I.size valsA < 1 = (keysB,valsB)
  | I.size valsB < 1 = (keysA,valsA)
  | otherwise = runST action
  where
  action :: forall s. ST s (karr k, varr v)
  action = do
    let !szA = I.size valsA
        !szB = I.size valsB
    !(keysDst :: Mutable karr s k) <- I.new (max szA szB * 8)
    !(valsDst :: Mutable varr s v) <- I.new (max szA szB * 4)
    let writeKeyRange :: Int -> k -> k -> ST s ()
        writeKeyRange !ix !lo !hi = do
          I.write keysDst (2 * ix) lo
          I.write keysDst (2 * ix + 1) hi
        writeDstHiKey :: Int -> k -> ST s ()
        writeDstHiKey !ix !hi = I.write keysDst (2 * ix + 1) hi
        writeDstValue :: Int -> v -> ST s ()
        writeDstValue !ix !v = I.write valsDst ix v
        readDstHiKey :: Int -> ST s k
        readDstHiKey !ix = I.read keysDst (2 * ix + 1)
        readDstVal :: Int -> ST s v
        readDstVal !ix = I.read valsDst ix
        indexLoKeyA :: Int -> k
        indexLoKeyA !ix = I.index keysA (ix * 2)
        indexLoKeyB :: Int -> k
        indexLoKeyB !ix = I.index keysB (ix * 2)
        indexHiKeyA :: Int -> k
        indexHiKeyA !ix = I.index keysA (ix * 2 + 1)
        indexHiKeyB :: Int -> k
        indexHiKeyB !ix = I.index keysB (ix * 2 + 1)
        indexValueA :: Int -> v
        indexValueA !ix = I.index valsA ix
        indexValueB :: Int -> v
        indexValueB !ix = I.index valsB ix
    -- In the go functon, ixDst is always at least one. Similarly,
    -- all key arguments are always greater than minBound.
    let go :: Int -> k -> k -> v -> Int -> k -> k -> v -> Int -> ST s Int
        go !ixA !loA !hiA !valA !ixB !loB !hiB !valB !ixDst = do
          prevHi <- readDstHiKey (ixDst - 1) 
          prevVal <- readDstVal (ixDst - 1) 
          case compare loA loB of
            LT -> do
              let (upper,ixA') = if hiA < loB
                    then (hiA,ixA + 1)
                    else (pred loB,ixA)
              ixDst' <- if pred loA == prevHi && valA == prevVal
                then do
                  writeDstHiKey (ixDst - 1) upper
                  return ixDst
                else do
                  writeKeyRange ixDst loA upper
                  writeDstValue ixDst valA
                  return (ixDst + 1)
              if ixA' < szA
                then do
                  let (loA',hiA') = if hiA < loB
                        then (indexLoKeyA ixA',indexHiKeyA ixA')
                        else (loB,hiA)
                  go ixA' loA' hiA' (indexValueA ixA') ixB loB hiB valB ixDst'
                else copyB ixB loB hiB valB ixDst'
            GT -> do
              let (upper,ixB') = if hiB < loA
                    then (hiB,ixB + 1)
                    else (pred loA,ixB)
              ixDst' <- if pred loB == prevHi && valB == prevVal
                then do
                  writeDstHiKey (ixDst - 1) upper
                  return ixDst
                else do
                  writeKeyRange ixDst loB upper
                  writeDstValue ixDst valB
                  return (ixDst + 1)
              if ixB' < szB
                then do
                  let (loB',hiB') = if hiB < loA
                        then (indexLoKeyB ixB',indexHiKeyB ixB')
                        else (loA,hiB)
                  go ixA loA hiA valA ixB' loB' hiB' (indexValueB ixB') ixDst'
                else copyA ixA loA hiA valA ixDst'
            EQ -> do
              let valCombination = combine valA valB
              case compare hiA hiB of
                LT -> do
                  ixDst' <- if pred loA == prevHi && valCombination == prevVal
                    then do
                      writeDstHiKey (ixDst - 1) hiA
                      return ixDst
                    else do
                      writeKeyRange ixDst loA hiA
                      writeDstValue ixDst valCombination
                      return (ixDst + 1)
                  let ixA' = ixA + 1
                      loB' = succ hiA
                  if ixA' < szA
                    then go ixA' (indexLoKeyA ixA') (indexHiKeyA ixA') (indexValueA ixA') ixB loB' hiB valB ixDst'
                    else copyB ixB loB' hiB valB ixDst'
                GT -> do
                  ixDst' <- if pred loB == prevHi && valCombination == prevVal
                    then do
                      writeDstHiKey (ixDst - 1) hiB
                      return ixDst
                    else do
                      writeKeyRange ixDst loB hiB
                      writeDstValue ixDst valCombination
                      return (ixDst + 1)
                  let ixB' = ixB + 1
                      loA' = succ hiB
                  if ixB' < szB
                    then go ixA loA' hiA valA ixB' (indexLoKeyB ixB') (indexHiKeyB ixB') (indexValueB ixB') ixDst'
                    else copyA ixA loA' hiA valA ixDst'
                EQ -> do
                  ixDst' <- if pred loB == prevHi && valCombination == prevVal
                    then do
                      writeDstHiKey (ixDst - 1) hiB
                      return ixDst
                    else do
                      writeKeyRange ixDst loB hiB
                      writeDstValue ixDst valCombination
                      return (ixDst + 1)
                  let ixA' = ixA + 1
                      ixB' = ixB + 1
                  if ixA' < szA
                    then if ixB' < szB
                      then go ixA' (indexLoKeyA ixA') (indexHiKeyA ixA') (indexValueA ixA') ixB' (indexLoKeyB ixB') (indexHiKeyB ixB') (indexValueB ixB') ixDst'
                      else copyA ixA' (indexLoKeyA ixA') (indexHiKeyA ixA') (indexValueA ixA') ixDst'
                    else if ixB' < szB
                      then copyB ixB' (indexLoKeyB ixB') (indexHiKeyB ixB') (indexValueB ixB') ixDst'
                      else return ixDst'
        copyB :: Int -> k -> k -> v -> Int -> ST s Int
        copyB !ixB !loB !hiB !valB !ixDst = do
          prevHi <- readDstHiKey (ixDst - 1) 
          prevVal <- readDstVal (ixDst - 1) 
          ixDst' <- if pred loB == prevHi && valB == prevVal
            then do
              writeDstHiKey (ixDst - 1) hiB
              return ixDst
            else do
              writeKeyRange ixDst loB hiB
              writeDstValue ixDst valB
              return (ixDst + 1)
          let ixB' = ixB + 1
              remaining = szB - ixB'
          I.copy keysDst (ixDst' * 2) keysB (ixB' * 2) (remaining * 2)
          I.copy valsDst ixDst' valsB ixB' remaining
          return (ixDst' + remaining)
        copyA :: Int -> k -> k -> v -> Int -> ST s Int
        copyA !ixA !loA !hiA !valA !ixDst = do
          prevHi <- readDstHiKey (ixDst - 1) 
          prevVal <- readDstVal (ixDst - 1) 
          ixDst' <- if pred loA == prevHi && valA == prevVal
            then do
              writeDstHiKey (ixDst - 1) hiA
              return ixDst
            else do
              writeKeyRange ixDst loA hiA
              writeDstValue ixDst valA
              return (ixDst + 1)
          let ixA' = ixA + 1
              remaining = szA - ixA'
          I.copy keysDst (ixDst' * 2) keysA (ixA' * 2) (remaining * 2)
          I.copy valsDst ixDst' valsA ixA' remaining
          return (ixDst' + remaining)
    let !loA0 = indexLoKeyA 0
        !loB0 = indexLoKeyB 0
        !hiA0 = indexHiKeyA 0
        !hiB0 = indexHiKeyB 0
        !valA0 = indexValueA 0
        !valB0 = indexValueB 0
    total <- case compare loA0 loB0 of
      LT -> if hiA0 < loB0
        then do
          writeKeyRange 0 loA0 hiA0
          writeDstValue 0 valA0
          if 1 < szA
            then go 1 (indexLoKeyA 1) (indexHiKeyA 1) (indexValueA 1) 0 loB0 hiB0 valB0 1
            else copyB 0 loB0 hiB0 valB0 1
        else do
          -- here we know that hiA > loA
          let !upperA = pred loB0
          writeKeyRange 0 loA0 upperA
          writeDstValue 0 valA0
          go 0 loB0 hiA0 valA0 0 loB0 hiB0 valB0 1
      EQ -> case compare hiA0 hiB0 of
        LT -> do
          writeKeyRange 0 loA0 hiA0
          writeDstValue 0 (combine valA0 valB0)
          if 1 < szA
            then go 1 (indexLoKeyA 1) (indexHiKeyA 1) (indexValueA 1) 0 (succ hiA0) hiB0 valB0 1
            else copyB 0 (succ hiA0) hiB0 valB0 1
        GT -> do
          writeKeyRange 0 loB0 hiB0
          writeDstValue 0 (combine valA0 valB0)
          if 1 < szB
            then go 0 (succ hiB0) hiA0 valA0 1 (indexLoKeyB 1) (indexHiKeyB 1) (indexValueB 1) 1
            else copyA 0 (succ hiB0) hiA0 valA0 1
        EQ -> do
          writeKeyRange 0 loA0 hiA0
          writeDstValue 0 (combine valA0 valB0)
          if 1 < szA
            then if 1 < szB
              then go 1 (indexLoKeyA 1) (indexHiKeyA 1) (indexValueA 1) 1 (indexLoKeyB 1) (indexHiKeyB 1) (indexValueB 1) 1
              else copyA 1 (indexLoKeyA 1) (indexHiKeyA 1) (indexValueA 1) 1
            else if 1 < szB
              then copyB 1 (indexLoKeyB 1) (indexHiKeyB 1) (indexValueB 1) 1
              else return 1
      GT -> if hiB0 < loA0
        then do
          writeKeyRange 0 loB0 hiB0
          writeDstValue 0 valB0
          if 1 < szB
            then go 0 loA0 hiA0 valA0 1 (indexLoKeyB 1) (indexHiKeyB 1) (indexValueB 1) 1
            else copyA 0 loA0 hiA0 valA0 1
        else do
          let !upperB = pred loA0
          writeKeyRange 0 loB0 upperB
          writeDstValue 0 valB0
          go 0 loA0 hiA0 valA0 0 loA0 hiB0 valB0 1
    !keysFinal <- I.resize keysDst (total * 2)
    !valsFinal <- I.resize valsDst total
    liftA2 (,) (I.unsafeFreeze keysFinal) (I.unsafeFreeze valsFinal)

concatWith :: forall karr varr k v. (Contiguous karr, Element karr k, Ord k, Enum k, Contiguous varr, Element varr v, Eq v)
  => (v -> v -> v)
  -> [Map karr varr k v]
  -> Map karr varr k v
concatWith combine = C.concatSized size empty (appendWith combine)

size :: (Contiguous varr, Element varr v) => Map karr varr k v -> Int
size (Map _ vals) = I.size vals 

toList :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v) => Map karr varr k v -> [(k,k,v)]
toList = foldrWithKey (\lo hi v xs -> (lo,hi,v) : xs) []

foldrWithKey :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v) => (k -> k -> v -> b -> b) -> b -> Map karr varr k v -> b
foldrWithKey f z (Map keys vals) =
  let !sz = I.size vals
      go !i
        | i == sz = z
        | otherwise =
            let !lo = I.index keys (i * 2)
                !hi = I.index keys (i * 2 + 1)
                !v = I.index vals i
             in f lo hi v (go (i + 1))
   in go 0

showsPrec :: (Contiguous karr, Element karr k, Show k, Contiguous varr, Element varr v, Show v) => Int -> Map karr varr k v -> ShowS
showsPrec p xs = showParen (p > 10) $
  showString "fromList " . shows (toList xs)

liftShowsPrec2 :: (Contiguous karr, Element karr k, Contiguous varr, Element varr v) => (Int -> k -> ShowS) -> ([k] -> ShowS) -> (Int -> v -> ShowS) -> ([v] -> ShowS) -> Int -> Map karr varr k v -> ShowS
liftShowsPrec2 showsPrecK _ showsPrecV _ p xs = showParen (p > 10) $
  showString "fromList " . showListWith (\(a,b,c) -> show_tuple [showsPrecK 0 a, showsPrecK 0 b, showsPrecV 0 c])  (toList xs)

-- implementation copied from GHC.Show
show_tuple :: [ShowS] -> ShowS
show_tuple ss = id
  . showChar '('
  . foldr1 (\s r -> s . showChar ',' . r) ss
  . showChar ')'


