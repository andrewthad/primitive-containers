{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Continuous.Set.Internal
  ( Set(..)
  , Inclusivity(..)
  , empty
  , universe
  , null
  , universal
  , singleton
  , append
  , member
  , equals
  , showsPrec
  ) where

import Prelude hiding (lookup,showsPrec,concat,map,foldr,negate,null)

import Control.Monad.ST (ST,runST)
import Data.Word (Word8)
import Data.Primitive.Contiguous (Contiguous,ContiguousU,Element,Mutable)
import Data.Primitive (PrimArray,MutablePrimArray)
import Data.Bits (unsafeShiftL,unsafeShiftR,(.|.),(.&.))
import qualified Prelude as P
import qualified Data.Primitive.Contiguous as I

-- Although the data constructor for this type is exported,
-- it isn't needed by anything in the continuous Set modules. It is needed
-- by the continuous Map modules to implement conversion functions.
--
-- All ranges in the set must be in order. Also, the set containing
-- everything must be represented with SetAll.
data Set arr a = Set
  -- (Maybe (Inclusivity,a)) -- negative infinity upper bound
  !(arr a) -- pairs of keys, last two keys are neg-inf upper and pos-inf lower, in that order
  !(PrimArray Word8) -- pairs of inclusive/exclusive, last element is edge information
  -- (Maybe (Inclusivity,a)) -- positive infinite lower bound

-- note: do not reorder these data constructors. Functions in
-- this module rely on the generated Ord instance.
data Inclusivity = Exclusive | Inclusive
  deriving (Eq,Ord,Show,Read)

data Edge
  = EdgeInclusive
  | EdgeExclusive
  | EdgeAbsent
  | EdgeUniversal

equals :: (Contiguous arr, Element arr a, Eq a) => Set arr a -> Set arr a -> Bool
equals (Set keys1 incs1) (Set keys2 incs2) =
  I.equals keys1 keys2 && incs1 == incs2

empty :: Contiguous arr => Set arr a
empty = Set I.empty $ runST $ do
  marr <- I.new 1
  I.write marr 0 (edgePairToWord8 EdgeAbsent EdgeAbsent)
  I.unsafeFreeze marr

universe :: Contiguous arr => Set arr a
universe = Set I.empty $ runST $ do
  marr <- I.new 1
  I.write marr 0 (edgePairToWord8 EdgeUniversal EdgeUniversal)
  I.unsafeFreeze marr

-- If the keys are null, then we know that there is only one
-- element in the inclusivity array.
null :: Contiguous arr => Set arr a -> Bool
null (Set keys incs) = I.null keys
  && I.index incs 0 == edgePairToWord8 EdgeAbsent EdgeAbsent

universal :: Contiguous arr => Set arr a -> Bool
universal (Set keys incs) = I.null keys
  && I.index incs 0 == edgePairToWord8 EdgeUniversal EdgeUniversal

singleton :: (Contiguous arr, Element arr a, Ord a)
  => Maybe (Inclusivity,a) -- ^ lower bound, @Nothing@ means @-∞@
  -> Maybe (Inclusivity,a) -- ^ upper bound, @Nothing@ means @+∞@
  -> Set arr a
singleton Nothing Nothing = universe
singleton Nothing (Just (incHi,hi)) = runST $ do
  keys <- I.replicateMut 1 hi >>= I.unsafeFreeze
  incs <- I.replicateMut 1 (edgePairToWord8 (inclusivityToEdge incHi) EdgeAbsent) >>= I.unsafeFreeze
  return (Set keys incs)
singleton (Just (incLo,lo)) Nothing = runST $ do
  keys <- I.replicateMut 1 lo >>= I.unsafeFreeze
  incs <- I.replicateMut 1 (edgePairToWord8 EdgeAbsent (inclusivityToEdge incLo)) >>= I.unsafeFreeze
  return (Set keys incs)
singleton (Just (incLo,lo)) (Just (incHi,hi)) = case compare lo hi of
  GT -> empty
  EQ -> if incLo == Inclusive && incHi == Inclusive
    then runST $ do
      keys <- I.replicateMut 2 lo >>= I.unsafeFreeze
      incsMut <- I.new 2
      I.write incsMut 0 (inclusivityPairToWord8 Inclusive Inclusive)
      I.write incsMut 1 (edgePairToWord8 EdgeAbsent EdgeAbsent)
      incs <- I.unsafeFreeze incsMut
      return (Set keys incs)
    else empty
  LT -> unsafeSingleton incLo lo incHi hi

-- the caller must ensure that lo is less than hi
unsafeSingleton :: (Contiguous arr, Element arr a) => Inclusivity -> a -> Inclusivity -> a -> Set arr a
unsafeSingleton incLo lo incHi hi = runST $ do
  keysMut <- I.replicateMut 2 lo
  I.write keysMut 1 hi
  keys <- I.unsafeFreeze keysMut
  incsMut <- I.new 2
  I.write incsMut 0 (inclusivityPairToWord8 incLo incHi)
  I.write incsMut 1 (edgePairToWord8 EdgeAbsent EdgeAbsent)
  incs <- I.unsafeFreeze incsMut
  return (Set keys incs)

except :: (Contiguous arr, Element arr a) => a -> Set arr a
except x = Set keys incs where
  keys = runST $ I.replicateMut 2 x >>= I.unsafeFreeze
  incs = runST $ do
    m <- I.new 1
    I.write m 0 (edgePairToWord8 EdgeExclusive EdgeExclusive)
    I.unsafeFreeze m

infinities :: (Contiguous arr, Element arr a, Ord a)
  => Inclusivity
  -> a -- ^ upper bound for negative infinity
  -> Inclusivity
  -> a -- ^ lower bound for positive infinite
  -> Set arr a
infinities negInfHiInc negInfHi posInfLoInc posInfLo =
  case compare negInfHi posInfLo of
    GT -> universe
    EQ -> if negInfHiInc == Exclusive && posInfLoInc == Exclusive
      then except negInfHi
      else universe
    LT -> unsafeInfinities negInfHiInc negInfHi posInfLoInc posInfLo

-- the caller must ensure that the upper bound for neg-inf is
-- less than the lower bound for pos inf
unsafeInfinities :: (Contiguous arr, Element arr a) => Inclusivity -> a -> Inclusivity -> a -> Set arr a
unsafeInfinities negInfHiInc negInfHi posInfLoInc posInfLo = runST $ do
  keysMut <- I.replicateMut 2 negInfHi
  I.write keysMut 1 posInfLo
  keys <- I.unsafeFreeze keysMut
  incsMut <- I.new 1
  I.write incsMut 0 (edgePairToWord8 (inclusivityToEdge negInfHiInc) (inclusivityToEdge posInfLoInc))
  incs <- I.unsafeFreeze incsMut
  return (Set keys incs)

append :: forall arr a. (Ord a, ContiguousU arr, Element arr a) => Set arr a -> Set arr a -> Set arr a
append s1@(Set keys1 incs1) s2@(Set keys2 incs2)
  | null s1 = s2
  | null s2 = s1
  | universal s1 = s1
  | universal s2 = s2
  | pairsCount1 == 0 && pairsCount2 == 0 = case lowerPair1 of
      Nothing -> case upperPair1 of
        Nothing -> error (errMsg 9)
        Just (posInfLoInc1,posInfLo1) -> case lowerPair2 of
          Nothing -> case upperPair2 of
            Just (posInfLoInc2,posInfLo2) -> case compare posInfLo1 posInfLo2 of
              EQ -> if posInfLoInc1 > posInfLoInc2 then s1 else s2
              GT -> s2
              LT -> s1
      Just (negInfHiInc1,negInfHi1) -> case upperPair1 of
        Nothing -> case lowerPair2 of
          Nothing -> case upperPair2 of
            Nothing -> error (errMsg 1)
            Just (posInfLoInc2,posInfLo2) ->
              case compare negInfHi1 posInfLo2 of
                GT -> universe
                EQ -> if negInfHiInc1 == Inclusive || posInfLoInc2 == Inclusive
                  then universe
                  else except negInfHi1
                LT -> unsafeInfinities negInfHiInc1 negInfHi1 posInfLoInc2 posInfLo2
          Just (negInfHiInc2,negInfHi2) -> case upperPair2 of
            Nothing -> case compare negInfHi1 negInfHi2 of
              EQ -> if negInfHiInc1 > negInfHiInc2 then s1 else s2
              GT -> s1
              LT -> s2
            Just (posInfLoInc2,posInfLo2) -> case compare negInfHi1 negInfHi2 of
              LT -> s2
              EQ -> if negInfHiInc1 > negInfHiInc2
                then infinities negInfHiInc1 negInfHi1 posInfLoInc2 posInfLo2
                else s2
              GT -> infinities negInfHiInc1 negInfHi1 posInfLoInc2 posInfLo2
  | otherwise = runST $ do
      let maxSz = pairsCount1 + pairsCount2 + 1
      keysMut <- I.new (maxSz * 2)
      incsMut <- I.new maxSz
      case lowerPairRes of
        Just (negInfHiIncOriginal,negInfHiOriginal) -> do
          let (negInfHiIncFinal,negInfHiFinal,ixInit1,ixInit2) = eatFromNegativeInfinity negInfHiIncOriginal negInfHiOriginal keys1 incs1 pairsCount1 keys2 incs2 pairsCount2
          case upperPairRes of
            Just (posInfLoIncOriginal,posInfLoOriginal) -> do
              let (posInfLoIncFinal,posInfLoFinal,ixLast1,ixLast2) = eatFromPositiveInfinity posInfLoIncOriginal posInfLoOriginal keys1 incs1 pairsCount1 keys2 incs2 pairsCount2
              finalIx <- go keysMut incsMut ixInit1 ixLast1 ixInit2 ixLast2 0 negInfHiIncFinal negInfHiFinal
              I.write incsMut finalIx (edgePairToWord8 (inclusivityToEdge negInfHiIncFinal) (inclusivityToEdge posInfLoIncFinal))
              I.write keysMut (finalIx * 2) negInfHiFinal
              I.write keysMut (finalIx * 2 + 1) posInfLoFinal
              keysFrozen <- I.resize keysMut (finalIx * 2 + 2) >>= I.unsafeFreeze
              incsFrozen <- I.resize incsMut (finalIx * 1) >>= I.unsafeFreeze
              return (Set keysFrozen incsFrozen)
            Nothing -> error (errMsg 102)
        Nothing -> error (errMsg 101)
  where
  -- do not make these patterns strict
  (lowerPair1,upperPair1,pairsCount1) = edges keys1 incs1
  (lowerPair2,upperPair2,pairsCount2) = edges keys2 incs2
  lowerPairRes = combineNegativeInfinities lowerPair1 lowerPair2
  upperPairRes = combineNegativeInfinities upperPair1 upperPair2
  go :: forall s.
        Mutable arr s a
     -> MutablePrimArray s Word8
     -> Int -- index 1
     -> Int -- index 1 last
     -> Int -- index 2
     -> Int -- index 2 last
     -> Int -- destination index
     -> Inclusivity -- previous inclusivity
     -> a -- previous destination value
     -> ST s Int -- returns size
  go !keysMut !incsMut !ix1 !ixLast1 !ix2 !ixLast2 !ixDst inc a = if ix1 <= ixLast1
    then error (errMsg 103)
    else if ix2 <= ixLast2
      then error (errMsg 104)
      else case upperPair1 of
        Just _ -> error (errMsg 105)
        Nothing -> case upperPair2 of
          Nothing -> return ixDst

combineNegativeInfinities :: Ord a => Maybe (Inclusivity,a) -> Maybe (Inclusivity,a) -> Maybe (Inclusivity,a)
combineNegativeInfinities Nothing Nothing = Nothing
combineNegativeInfinities Nothing x@(Just _) = x
combineNegativeInfinities x@(Just _) Nothing = x
combineNegativeInfinities (Just (xinc,x)) (Just (yinc,y)) = case compare x y of
  GT -> Just (xinc,x)
  LT -> Just (yinc,y)
  EQ -> Just (max xinc yinc,y)

eatFromPositiveInfinity ::
     Inclusivity -- inclusivity for positive infinity
  -> a -- lower bound for positive infinity
  -> arr a -- set 1
  -> PrimArray Word8
  -> Int -- pairs in set 1
  -> arr a -- set 2
  -> PrimArray Word8
  -> Int -- pairs in set 2
  -> (Inclusivity,a,Int,Int) -- index for set 1 and set2, lower bound for positive infinity
eatFromPositiveInfinity = error (errMsg 110)

eatFromNegativeInfinity :: (Contiguous arr, Element arr a, Ord a)
  => Inclusivity -- inclusivity for negative infinity
  -> a -- upper bound for negative infinity
  -> arr a -- set 1
  -> PrimArray Word8
  -> Int -- pairs in set 1
  -> arr a -- set 2
  -> PrimArray Word8
  -> Int -- pairs in set 2
  -> (Inclusivity,a,Int,Int) -- index for set 1 and set2, upper bound for negative infinity
eatFromNegativeInfinity negInfInc0 negInfHi0 keys1 incs1 sz1 keys2 incs2 sz2 = go negInfInc0 negInfHi0 0 0
  where
  go negInfHiInc negInfHi !ix1 !ix2 = if ix1 < sz1
    then error (errMsg 111)
    else if ix2 < sz2
      then let (# lo #) = I.index# keys2 (ix2 * 2)
               (# hi #) = I.index# keys2 (ix2 * 2 + 1)
               (loInc,hiInc) = indexInclusivityPair incs2 ix2
            in case compare negInfHi lo of
                 LT -> (negInfHiInc,negInfHi,ix1,ix2)
                 GT -> case compare negInfHi hi of
                   LT -> go hiInc hi ix1 (ix2 + 1)
                   GT -> go negInfHiInc negInfHi ix1 (ix2 + 1)
                   EQ -> go (max hiInc negInfHiInc) hi ix1 (ix2 + 1)
                 EQ -> if negInfHiInc == Exclusive && loInc == Exclusive
                   then (Exclusive,negInfHi,ix1,ix2)
                   else case compare negInfHi hi of
                     LT -> go hiInc hi ix1 (ix2 + 1)
                     GT -> go negInfHiInc negInfHi ix1 (ix2 + 1)
                     EQ -> go (max hiInc negInfHiInc) hi ix1 (ix2 + 1)
      else (negInfHiInc,negInfHi,ix1,ix2)

inclusivityToEdge :: Inclusivity -> Edge
inclusivityToEdge Inclusive = EdgeInclusive
inclusivityToEdge Exclusive = EdgeExclusive

inclusivityToWord8 :: Inclusivity -> Word8
inclusivityToWord8 Inclusive = 0
inclusivityToWord8 Exclusive = 1

inclusivityPairToWord8 :: Inclusivity -> Inclusivity -> Word8
inclusivityPairToWord8 a b =
      unsafeShiftL (inclusivityToWord8 a) 1
  .|. inclusivityToWord8 b

word8ToInclusivity :: Word8 -> Inclusivity
word8ToInclusivity 0 = Inclusive
word8ToInclusivity _ = Exclusive

indexInclusivityPair :: PrimArray Word8 -> Int -> (Inclusivity,Inclusivity)
indexInclusivityPair xs ix = case I.index xs ix of
  0 -> (Inclusive,Inclusive)
  1 -> (Inclusive,Exclusive)
  2 -> (Exclusive,Inclusive)
  _ -> (Exclusive,Exclusive)

edgeToWord8 :: Edge -> Word8
edgeToWord8 EdgeInclusive = 0
edgeToWord8 EdgeExclusive = 1
edgeToWord8 EdgeAbsent = 2
edgeToWord8 EdgeUniversal = 3

word8ToEdge :: Word8 -> Edge
word8ToEdge x = case x of
  0 -> EdgeInclusive
  1 -> EdgeExclusive
  2 -> EdgeAbsent
  _ -> EdgeUniversal

edgePairToWord8 :: Edge -> Edge -> Word8
edgePairToWord8 a b = unsafeShiftL (edgeToWord8 a) 2 .|. edgeToWord8 b

edgeMetadata :: PrimArray Word8 -> (Edge,Edge)
edgeMetadata xs = (word8ToEdge (unsafeShiftR w 2), word8ToEdge (0b00000011 .&. w))
  where
  w = I.index xs (I.size xs - 1)

-- please check for EdgeUniversal before calling this function. The
-- resulting triple includes the size of the keys array in pairs. The
-- divisions used internally here should always divide two evenly.
edges :: (Contiguous arr, Element arr a)
  => arr a
  -> PrimArray Word8
  -> (Maybe (Inclusivity,a), Maybe (Inclusivity,a),Int)
edges keys incs = case edgeMetadata incs of
  (lower,upper) -> case lower of
    EdgeUniversal -> error (errMsg 2)
    EdgeAbsent -> case upper of
      EdgeInclusive -> (Nothing,Just (Inclusive,I.index keys (sz - 1)),div (sz - 1) 2)
      EdgeExclusive -> (Nothing,Just (Exclusive,I.index keys (sz - 1)),div (sz - 1) 2)
      EdgeAbsent -> (Nothing,Nothing,div sz 2)
      _ -> error (errMsg 3)
    EdgeInclusive -> case upper of
      EdgeInclusive -> (Just (Inclusive,I.index keys (sz - 2)),Just (Inclusive,I.index keys (sz - 1)),div (sz - 2) 2)
      EdgeExclusive -> (Just (Inclusive,I.index keys (sz - 2)),Just (Exclusive,I.index keys (sz - 1)),div (sz - 2) 2)
      EdgeAbsent -> (Just (Inclusive,I.index keys (sz - 1)),Nothing,div (sz - 1) 2)
      EdgeUniversal -> error (errMsg 4)
    EdgeExclusive -> case upper of
      EdgeInclusive -> (Just (Exclusive,I.index keys (sz - 2)),Just (Inclusive,I.index keys (sz - 1)),div (sz - 2) 2)
      EdgeExclusive -> (Just (Exclusive,I.index keys (sz - 2)),Just (Exclusive,I.index keys (sz - 1)),div (sz - 2) 2)
      EdgeAbsent -> (Just (Exclusive,I.index keys (sz - 1)),Nothing,div (sz - 1) 2)
      EdgeUniversal -> error (errMsg 5)
  where
  sz = I.size keys

member :: forall arr a. (Contiguous arr, Element arr a, Ord a)
  => a
  -> Set arr a
  -> Bool
member val (Set keys incs) = case edges keys incs of
  (!mnegInfHi,!mposInfLo,!n) ->
    case mnegInfHi of
      Nothing -> case mposInfLo of
        Nothing -> go 0 (n - 1)
        Just (!posInfLoInc,!posInfLo) -> case compare val posInfLo of
          GT -> True
          LT -> go 0 (n - 1)
          EQ -> posInfLoInc == Inclusive
      Just (!negInfHiInc,!negInfHi) -> case mposInfLo of
        Nothing -> case compare val negInfHi of
          LT -> True
          GT -> go 0 (n - 1)
          EQ -> negInfHiInc == Inclusive
        Just (!posInfLoInc,!posInfLo) -> case compare val posInfLo of
          GT -> True
          LT -> case compare val negInfHi of
            GT -> go 0 (n - 1)
            LT -> True
            EQ -> negInfHiInc == Inclusive
          EQ -> posInfLoInc == Inclusive
  where
  go !start !end = if end <= start
    then if end == start
      then
        let !(# valLo #) = I.index# keys (2 * start)
            !(# valHi #) = I.index# keys (2 * start + 1)
         in case indexInclusivityPair incs start of
              (Exclusive,Exclusive) -> val > valLo && val < valHi
              (Exclusive,Inclusive) -> val > valLo && val <= valHi
              (Inclusive,Exclusive) -> val >= valLo && val < valHi
              (Inclusive,Inclusive) -> val >= valLo && val <= valHi
      else False
    else
      let !mid = div (end + start + 1) 2
          !valLo = I.index keys (2 * mid)
       in case P.compare val valLo of
            LT -> go start (mid - 1)
            EQ -> True
            GT -> go mid end
{-# INLINEABLE member #-}

errMsg :: Int -> String
errMsg n = "Data.Continuous.Set.Internal: invariant " ++ show n ++ " violated"

toPairs :: (Contiguous arr, Element arr a) => Int -> Set arr a -> [(Inclusivity,a,Inclusivity,a)]
toPairs n (Set keys incs) = go 0 where
  go !ix = if ix < n
    then
      let (incLo,incHi) = indexInclusivityPair incs ix
          lo = I.index keys (2 * ix)
          hi = I.index keys (2 * ix + 1)
       in (incLo,lo,incHi,hi) : go (ix + 1)
    else []

showsPrec :: (Contiguous arr, Element arr a, Show a)
  => Int
  -> Set arr a
  -> ShowS
showsPrec _ s@(Set keys incs)
  | null s = showString "{}"
  | universal s = showString "{(-∞,+∞)}"
  | otherwise = showChar '{' . showListInf shows lowerPair (toPairs pairsCount s) upperPair . showChar '}'
  where
  -- do not make these patterns strict
  (lowerPair,upperPair,pairsCount) = edges keys incs

showListInf :: (a -> ShowS) -> Maybe (Inclusivity,a) -> [(Inclusivity,a,Inclusivity,a)] -> Maybe (Inclusivity,a) -> ShowS
showListInf showx mnegInfHi [] mposInfLo s = case mnegInfHi of
  Nothing -> case mposInfLo of
    Nothing -> s
    Just (posInfLoInc,posInfLo) -> showPosInfLo showx posInfLoInc posInfLo s
  Just (negInfHiInc,negInfHi) -> case mposInfLo of
    Nothing -> showNegInfHi showx negInfHiInc negInfHi s
    Just (posInfLoInc,posInfLo) -> showChar '{'
      $ showNegInfHi showx negInfHiInc negInfHi
      $ showChar ','
      $ showPosInfLo showx posInfLoInc posInfLo
      $ showChar '}'
      $ s
showListInf showx mnegInfHi ((ainc0,a0,binc0,b0):xs) mposInfLo s =
  maybe id (\(negInfHiInc,negInfHi) s' -> showNegInfHi showx negInfHiInc negInfHi (',' : s')) mnegInfHi (case ainc0 of {Inclusive -> '[';Exclusive -> '('} : showx a0 (',' : showx b0 (case binc0 of {Inclusive -> ']'; Exclusive -> ')'} : showl xs)))
  where
    showl [] = maybe id (\(posInfLoInc,posInfLo) -> showChar ',' . showPosInfLo showx posInfLoInc posInfLo) mposInfLo (']' : s)
    showl ((ainc,a,binc,b):ys) = ',' : case ainc of {Inclusive -> '[';Exclusive -> '('} : showx a (',' : showx b (case binc of {Inclusive -> ']'; Exclusive -> ')'} : showl ys))

showNegInfHi :: (a -> ShowS) -> Inclusivity -> a -> ShowS
showNegInfHi showx inc x s = "(-∞," ++ showx x ((case inc of { Inclusive -> ']'; Exclusive -> ')'} : s))

showPosInfLo :: (a -> ShowS) -> Inclusivity -> a -> ShowS
showPosInfLo showx inc x s = case inc of { Inclusive -> '['; Exclusive -> '('} : (showx x (",+∞)" ++ s))

