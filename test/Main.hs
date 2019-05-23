{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Primitive
import Data.Word
import Data.Int

import Control.Applicative (liftA2)
import Control.Monad (forM)
import Data.Bool (bool)
import Data.Continuous.Set.Lifted (Inclusivity(..))
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Primitive.Unlifted.Class (PrimUnlifted)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup)
import Test.HUnit.Base (assertEqual)
import Test.QuickCheck (Arbitrary,Gen,(===),(==>))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit (testCase,(@?=))
import Text.Read (readMaybe)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text as T
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Classes as QCC
import qualified Data.Semigroup as SG
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Foldable as F
import qualified GHC.Exts as E
import qualified Test.QuickCheck.Classes.IsList as QCCL

import qualified Data.Set.Unboxed as SU
import qualified Data.Set.Lifted as SL
import qualified Data.Set.Unlifted as SUL
import qualified Data.Map.Lifted.Lifted as MLL
import qualified Data.Map.Unboxed.Lifted as MUL
import qualified Data.Map.Unboxed.Unboxed as MUU
import qualified Data.Diet.Map.Strict.Unboxed.Lifted as DMUL
import qualified Data.Diet.Map.Strict.Lifted.Lifted as DMLL
import qualified Data.Diet.Set.Lifted as DSL
import qualified Data.Continuous.Set.Lifted as CSL
import qualified Data.Diet.Unbounded.Set.Lifted as DUSL
import qualified Data.Map.Subset.Strict.Lifted as MSL
import qualified Data.Map.Interval.DBTSLL as MIDBTS

main :: IO ()
main = defaultMain $ testGroup "Data"
  [ testGroup "Set"
    [ testGroup "Unboxed"
      [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (SU.Set Int16)))
      , lawsToTest (QCC.ordLaws (Proxy :: Proxy (SU.Set Int16)))
      , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (SU.Set Int16)))
      , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (SU.Set Int16)))
      , lawsToTest (QCC.isListLaws (Proxy :: Proxy (SU.Set Int16)))
      , TQC.testProperty "member" (memberProp @Int16 E.fromList SU.member)
      , TQC.testProperty "tripleton" setTripletonProp
      , TQC.testProperty "intersects" intersectsWorksProp
      ]
    , testGroup "Lifted"
      [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (SL.Set Integer)))
      , lawsToTest (QCC.ordLaws (Proxy :: Proxy (SL.Set Integer)))
      , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (SL.Set Integer)))
      , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (SL.Set Integer)))
      , lawsToTest (QCC.isListLaws (Proxy :: Proxy (SL.Set Integer)))
      , TQC.testProperty "member" (memberProp @Integer E.fromList SL.member)
      , TQC.testProperty "nonMember" (nonMemberProp E.fromList SL.member)
      , TQC.testProperty "foldr" (QCCL.foldrProp int32 SL.foldr)
      , TQC.testProperty "foldl'" (QCCL.foldlProp int16 SL.foldl')
      , TQC.testProperty "foldr'" (QCCL.foldrProp int32 SL.foldr')
      , TQC.testProperty "foldMap" foldMapSetProp
      , TQC.testProperty "foldMap'" foldMapStrictSetProp
      , TQC.testProperty "difference" differenceProp
      , TQC.testProperty "intersection" intersectionProp
      , TQC.testProperty "traverse_" traverseSetProp
      , TQC.testProperty "itraverse_" itraverseSetProp
      ]
    , testGroup "Unlifted"
      [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (SUL.Set (PrimArray Int16))))
      , lawsToTest (QCC.ordLaws (Proxy :: Proxy (SUL.Set (PrimArray Int16))))
      , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (SUL.Set (PrimArray Int16))))
      , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (SUL.Set (PrimArray Int16))))
      , lawsToTest (QCC.isListLaws (Proxy :: Proxy (SUL.Set (PrimArray Int16))))
      , TQC.testProperty "member" (memberProp @(PrimArray Int16) E.fromList SUL.member)
      ]
    ]
  , testGroup "Map"
    [ testGroup "Unboxed"
      [ testGroup "Unboxed"
        [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (MUU.Map Word32 Int)))
        , lawsToTest (QCC.ordLaws (Proxy :: Proxy (MUU.Map Word32 Int)))
        , lawsToTest (QCC.semigroupLaws (Proxy :: Proxy (MUU.Map Word32 Word)))
        , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (MUU.Map Word32 Int)))
        , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (MUU.Map Word32 Int)))
        , lawsToTest (QCC.isListLaws (Proxy :: Proxy (MUU.Map Word32 Int)))
        , TQC.testProperty "lookup" (lookupProp @Word32 @Int E.fromList MUU.lookup)
        , TQC.testProperty "foldlWithKey'" (mapFoldAgreement MUU.foldlWithKey' M.foldlWithKey)
        , TQC.testProperty "foldrWithKey'" (mapFoldAgreement MUU.foldrWithKey' M.foldrWithKey)
        , TQC.testProperty "foldMapWithKey'" (mapFoldMonoidAgreement MUU.foldMapWithKey' M.foldMapWithKey)
        , TQC.testProperty "mapMaybe" mapMaybeProp
        ]
      , testGroup "Lifted"
        [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (MUL.Map Word32 Integer)))
        , lawsToTest (QCC.ordLaws (Proxy :: Proxy (MUL.Map Word32 Integer)))
        , lawsToTest (QCC.semigroupLaws (Proxy :: Proxy (MUL.Map Word32 Integer)))
        , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (MUL.Map Word32 Integer)))
        , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (MUL.Map Word32 Integer)))
        , lawsToTest (QCC.isListLaws (Proxy :: Proxy (MUL.Map Word32 Integer)))
        , TQC.testProperty "lookup-empty" lookupEmptyUnboxedLiftedMapProp
        , TQC.testProperty "mapWithKey" mapWithKeyProp
        , TQC.testProperty "appendWithKey" appendWithKeyUnboxedLiftedProp
        ]
      ]
    , testGroup "Lifted"
      [ testGroup "Lifted"
        [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (MLL.Map Integer Integer)))
        , lawsToTest (QCC.ordLaws (Proxy :: Proxy (MLL.Map Integer Integer)))
        , lawsToTest (QCC.semigroupLaws (Proxy :: Proxy (MLL.Map Integer Integer)))
        , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (MLL.Map Integer Integer)))
        , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (MLL.Map Integer Integer)))
        , lawsToTest (QCC.isListLaws (Proxy :: Proxy (MLL.Map Integer Integer)))
        , TQC.testProperty "appendWithKey" appendWithKeyLiftedLiftedProp
        ]
      ]
    , testGroup "Interval"
      [ testGroup "DBTS"
        [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (MIDBTS.Map Word8 Integer)))
        , lawsToTest (QCC.semigroupLaws (Proxy :: Proxy (MIDBTS.Map Word8 (S.Set Integer))))
        , lawsToTest (QCC.commutativeSemigroupLaws (Proxy :: Proxy (MIDBTS.Map Word8 (S.Set Integer))))
        , lawsToTest (QCC.idempotentSemigroupLaws (Proxy :: Proxy (MIDBTS.Map Word8 (S.Set Integer))))
        , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (MIDBTS.Map Word8 Integer)))
        , lawsToTest (QCC.isListLaws (Proxy :: Proxy (MIDBTS.Map Word8 Integer)))
        , TQC.testProperty "lookup" dbtsIntervalMapLookupProp
        , testGroup "Unit"
          [ testCase "A" $ do
              let s = MIDBTS.singleton 102 (1 :: Word8) (2 :: Word8) (101 :: Integer)
              show s @?= "fromList [(0,0,102),(1,2,101),(3,255,102)]"
          , testCase "B" $ do
              let s = MIDBTS.singleton 102 (2 :: Word8) (2 :: Word8) (101 :: Integer)
              show s @?= "fromList [(0,1,102),(2,2,101),(3,255,102)]"
          , testCase "C" $ do
              let s = MIDBTS.singleton 102 (0 :: Word8) (0 :: Word8) (101 :: Integer)
              show s @?= "fromList [(0,0,101),(1,255,102)]"
          , testCase "D" $ do
              let s = MIDBTS.fromList 102 [(1 :: Word8, 2 :: Word8, 100 :: Integer),(5,7,101)]
              show s @?= "fromList [(0,0,102),(1,2,100),(3,4,102),(5,7,101),(8,255,102)]"
          , testCase "E" $ do
              let s = MIDBTS.fromList 102 [(5,7,101),(1 :: Word8, 2 :: Word8, 100 :: Integer)]
              show s @?= "fromList [(0,0,102),(1,2,100),(3,4,102),(5,7,101),(8,255,102)]"
          ]
        ]
      ]
    ]
  , testGroup "Continuous"
    [ testGroup "Set"
      [ testGroup "Lifted"
        [ testGroup "Unit" 
          [ testCase "A" $ do
              let s = CSL.singleton Nothing (Just (Inclusive,55 :: Integer))
                      <>
                      CSL.singleton (Just (Exclusive,200 :: Integer)) Nothing
                  str = show s
              assertEqual (str ++ " contains 50") (CSL.member 50 s) True
              assertEqual (str ++ " contains 270") (CSL.member 270 s) True
              assertEqual (str ++ " contains 55") (CSL.member 55 s) True
              assertEqual (str ++ " does not contain 200") (CSL.member 200 s) False
              assertEqual (str ++ " does not contain 56") (CSL.member 56 s) False
          , testCase "B" $ do
              let s = CSL.singleton Nothing (Just (Inclusive,14 :: Integer))
                      <>
                      CSL.singleton (Just (Exclusive,14 :: Integer)) Nothing
              s @?= CSL.universe
          , testCase "C" $ do
              let s = CSL.singleton Nothing (Just (Exclusive,14 :: Integer))
                      <>
                      CSL.singleton (Just (Exclusive,14 :: Integer)) Nothing
                  str = show s
              assertEqual (str ++ " does not contain 14") (CSL.member 14 s) False
          ]
        ]
      ]
    ]
  , testGroup "Diet"
    [ testGroup "Unbounded"
      [ testGroup "Set"
        [ testGroup "Lifted"
          [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (DUSL.Set Word8)))
          , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (DUSL.Set Word8)))
          , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (DUSL.Set Word8)))
          ]
        ]
      ]
    , testGroup "Set"
      [ testGroup "Lifted"
        [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (DSL.Set Word16)))
        , lawsToTest (QCC.ordLaws (Proxy :: Proxy (DSL.Set Word16)))
        , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (DSL.Set Word16)))
        , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (DSL.Set Word16)))
        , lawsToTest (QCC.isListLaws (Proxy :: Proxy (DSL.Set Word16)))
        , TQC.testProperty "member" (dietMemberProp @Word8 E.fromList DSL.member)
        , TQC.testProperty "difference" dietSetDifferenceProp
        , TQC.testProperty "intersection" dietSetIntersectionProp
        , TQC.testProperty "negate" dietSetNegateProp
        , TQC.testProperty "aboveInclusive" dietSetAboveProp
        , testGroup "belowInclusive"
          [ TQC.testProperty "basic" dietSetBelowProp
          , TQC.testProperty "lowest" dietSetBelowLowestProp
          , TQC.testProperty "highest" dietSetBelowHighestProp
          ]
        , testGroup "betweenInclusive"
          [ TQC.testProperty "basic" dietSetBetweenProp
          , TQC.testProperty "border" dietSetBetweenBorderProp
          , TQC.testProperty "inside" dietSetBetweenBorderNearProp
          ]
        ]
      ]
    , testGroup "Map"
      [ testGroup "Subset"
        [ testGroup "Lifted"
          [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (MSL.Map Integer (SG.Sum Integer))))
          , lawsToTest (QCC.semigroupLaws (Proxy :: Proxy (MSL.Map Integer (SG.First Integer))))
          , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (MSL.Map Integer (SG.Sum Integer))))
          , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (MSL.Map Integer (SG.Sum Integer))))
          , TQC.testProperty "lookup" subsetMapLookupProp
          ]
        ]
      , testGroup "Lifted"
        [ testGroup "Lifted"
          [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (DMLL.Map Word8 Integer)))
          , lawsToTest (QCC.semigroupLaws (Proxy :: Proxy (DMLL.Map Word8 Word)))
          , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (DMLL.Map Word8 Int)))
          , lawsToTest (QCC.isListLaws (Proxy :: Proxy (DMLL.Map Word8 Integer)))
          , TQC.testProperty "lookup" (dietLookupPropA @Word8 @Int E.fromList DMLL.lookup)
          , TQC.testProperty "doubleton" dietDoubletonProp
          , TQC.testProperty "valid" dietValidProp
          ]
        ]
      , testGroup "Unboxed"
        [ testGroup "Lifted"
          [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (DMUL.Map Word8 Integer)))
          , lawsToTest (QCC.semigroupLaws (Proxy :: Proxy (DMUL.Map Word8 Word)))
          , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (DMUL.Map Word8 Int)))
          , lawsToTest (QCC.isListLaws (Proxy :: Proxy (DMUL.Map Word8 Integer)))
          , TQC.testProperty "lookup" (dietLookupPropA @Word32 @Int E.fromList DMUL.lookup)
          ]
        ]
      ]
    ]
  ]

int16 :: Proxy Int16
int16 = Proxy

int32 :: Proxy Int32
int32 = Proxy

subsetMapLookupProp :: QC.Property
subsetMapLookupProp = QC.property $ \(xs :: MSL.Map Integer Integer) ->
  let xs' = MSL.toList xs
   in all (\(k,v) -> MSL.lookup k xs == Just v) xs' === True

dietSetDifferenceProp :: QC.Property
dietSetDifferenceProp = QC.property $ \(xs :: DSL.Set Word8) (ys :: DSL.Set Word8) ->
  let xs' = dietSetToSet xs
      ys' = dietSetToSet ys
   in DSL.difference xs ys === DSL.fromList (map (\x -> (x,x)) (S.toList (S.difference xs' ys')))

dietSetIntersectionProp :: QC.Property
dietSetIntersectionProp = QC.property $ \(xs :: DSL.Set Word8) (ys :: DSL.Set Word8) ->
  let xs' = dietSetToSet xs
      ys' = dietSetToSet ys
   in DSL.intersection xs ys === DSL.fromList (map (\x -> (x,x)) (S.toList (S.intersection xs' ys')))

dietSetNegateProp :: QC.Property
dietSetNegateProp = QC.property $ \(xs :: DSL.Set Word8) ->
  let xs' = dietSetToSet xs
      expected = foldMap (\n -> bool (S.singleton n) mempty (S.member n xs')) [minBound..maxBound]
   in DSL.negate xs === mconcat (map (\x -> DSL.singleton x x) (F.toList expected))

dietSetAboveProp :: QC.Property
dietSetAboveProp = QC.property $ \(y :: Word8) (ys :: DSL.Set Word8) ->
  let ys' = dietSetToSet ys
      (_,isMember,c) = S.splitMember y ys'
      r = if isMember then S.insert y c else c
   in DSL.aboveInclusive y ys === DSL.fromList (map (\x -> (x,x)) (S.toList r))

dietSetBelowProp :: QC.Property
dietSetBelowProp = QC.property $ \(y :: Word8) (ys :: DSL.Set Word8) ->
  let ys' = dietSetToSet ys
      (c,isMember,_) = S.splitMember y ys'
      r = if isMember then S.insert y c else c
   in DSL.belowInclusive y ys === DSL.fromList (map (\x -> (x,x)) (S.toList r))

dietSetBelowLowestProp :: QC.Property
dietSetBelowLowestProp = QC.property $ \(ys :: DSL.Set Word8) ->
  let ys' = dietSetToSet ys
   in case S.lookupMin ys' of
        Nothing -> QC.property QC.Discard
        Just y -> 
          let (c,isMember,_) = S.splitMember y ys'
              r = if isMember then S.insert y c else c
           in QC.property (DSL.belowInclusive y ys === DSL.fromList (map (\x -> (x,x)) (S.toList r)))

dietSetBelowHighestProp :: QC.Property
dietSetBelowHighestProp = QC.property $ \(ys :: DSL.Set Word8) ->
  let ys' = dietSetToSet ys
   in case S.lookupMax ys' of
        Nothing -> QC.property QC.Discard
        Just y -> 
          let (c,isMember,_) = S.splitMember y ys'
              r = if isMember then S.insert y c else c
           in QC.property (DSL.belowInclusive y ys === DSL.fromList (map (\x -> (x,x)) (S.toList r)))

dietSetBetweenProp :: QC.Property
dietSetBetweenProp = QC.property $ \(x :: Word8) (y :: Word8) (ys :: DSL.Set Word8) ->
  (x <= y)
  ==> 
  ( let ys' = dietSetToSet ys
        r = S.filter (\e -> e >= x && e <= y) ys'
     in DSL.betweenInclusive x y ys === DSL.fromList (map (\z -> (z,z)) (S.toList r))
  )

dietSetBetweenBorderProp :: QC.Property
dietSetBetweenBorderProp = QC.property $ \(ys :: DSL.Set Word8) ->
  let ys' = dietSetToSet ys
   in case S.lookupMax ys' of
        Nothing -> QC.property QC.Discard
        Just hi -> case S.lookupMin ys' of
          Nothing -> QC.property QC.Discard
          Just lo -> 
            let r = S.filter (\e -> e >= lo && e <= hi) ys'
             in DSL.betweenInclusive lo hi ys === DSL.fromList (map (\z -> (z,z)) (S.toList r))

dietSetBetweenBorderNearProp :: QC.Property
dietSetBetweenBorderNearProp = QC.property $ \(ys :: DSL.Set Word8) ->
  let ys' = dietSetToSet ys
   in ( S.size ys' > 1
        ==>
        ( let hi = pred (S.findMax ys')
              lo = succ (S.findMin ys')
              r = S.filter (\e -> e >= lo && e <= hi) ys'
           in DSL.betweenInclusive lo hi ys === DSL.fromList (map (\z -> (z,z)) (S.toList r))
        )
      )

-- This enumerates all of the element contained by all ranges
-- in the diet set.
dietSetToSet :: (Enum a, Ord a) => DSL.Set a -> S.Set a
dietSetToSet = DSL.foldr
  (\lo hi s -> S.fromList (enumFromTo lo hi) SG.<> s)
  mempty

differenceProp :: QC.Property
differenceProp = QC.property $ \(xs :: S.Set Word8) (ys :: S.Set Word8) ->
  let xs' = SL.fromList (S.toList xs)
      ys' = SL.fromList (S.toList ys)
   in SL.toList (SL.difference xs' ys') === S.toList (S.difference xs ys)

intersectionProp :: QC.Property
intersectionProp = QC.property $ \(xs :: S.Set Word8) (ys :: S.Set Word8) ->
  let xs' = SL.fromList (S.toList xs)
      ys' = SL.fromList (S.toList ys)
   in SL.toList (SL.intersection xs' ys') === S.toList (S.intersection xs ys)

traverseSetProp :: QC.Property
traverseSetProp = QC.property $ \(xs :: S.Set Word8) ->
  let xs' = SL.fromList (S.toList xs)
   in SL.traverse_ (Const . SG.Sum) xs' === F.traverse_ (Const . SG.Sum) xs

foldMapSetProp :: QC.Property
foldMapSetProp = QC.property $ \(xs :: S.Set Word8) ->
  let xs' = SL.fromList (S.toList xs)
   in SL.foldMap SG.Sum xs' === F.foldMap SG.Sum xs

foldMapStrictSetProp :: QC.Property
foldMapStrictSetProp = QC.property $ \(xs :: S.Set Word8) ->
  let xs' = SL.fromList (S.toList xs)
   in SL.foldMap' SG.Sum xs' === F.foldMap SG.Sum xs

mapMaybeProp :: QC.Property
mapMaybeProp = QC.property $ \(xs :: M.Map Word8 Word8) ->
  let xs' = MUU.fromList (M.toList xs)
      func x = if even x then Just (x * x) else Nothing
   in MUU.toList (MUU.mapMaybe func xs') === M.toList (M.mapMaybe func xs)

mapWithKeyProp :: QC.Property
mapWithKeyProp = QC.property $ \(xs :: M.Map Word8 Word8) ->
  let xs' = MUL.fromList (M.toList xs)
      func x y = if even x then y * x else x + 1
   in MUL.toList (MUL.mapWithKey func xs') === M.toList (M.mapWithKey func xs)

appendWithKeyUnboxedLiftedProp :: QC.Property
appendWithKeyUnboxedLiftedProp = QC.property $ \(xs :: M.Map Word8 Word8) ys ->
  let xs' = MUL.fromList (M.toList xs)
      ys' = MUL.fromList (M.toList ys) 
      func k x y = k + 2 * x + 3 * y
   in MUL.toList (MUL.appendWithKey func xs' ys') === M.toList (M.unionWithKey func xs ys)

appendWithKeyLiftedLiftedProp :: QC.Property
appendWithKeyLiftedLiftedProp = QC.property $ \(xs :: M.Map Word8 Word8) ys ->
  let xs' = MLL.fromList (M.toList xs)
      ys' = MLL.fromList (M.toList ys) 
      func k x y = k + 2 * x + 3 * y
   in MLL.toList (MLL.appendWithKey func xs' ys') === M.toList (M.unionWithKey func xs ys)

itraverseSetProp :: QC.Property
itraverseSetProp = QC.property $ \(xs :: S.Set Int) ->
  let xs' = SL.fromList (S.toList xs)
      zs = zip (enumFrom (0 :: Int)) (S.toList xs)
   in SL.itraverse_ (\ix x -> Const (SG.Sum (ix + x))) xs' === F.traverse_ (\(ix,x) -> Const (SG.Sum (ix + x))) zs

mapFoldMonoidAgreement ::
     ((Int -> Int -> [Int]) -> MUU.Map Int Int -> [Int])
  -> ((Int -> Int -> [Int]) -> M.Map Int Int -> [Int])
  -> QC.Property
mapFoldMonoidAgreement foldPrim foldContainer = QC.property $ \(xs :: [(Int,Int)]) ->
  let p = E.fromList xs
      c = E.fromList xs
      func x y = [x + y]
   in foldPrim func p === foldContainer func c

mapFoldAgreement ::
     ((Int -> Int -> Int -> Int) -> Int -> MUU.Map Int Int -> Int)
  -> ((Int -> Int -> Int -> Int) -> Int -> M.Map Int Int -> Int)
  -> QC.Property
mapFoldAgreement foldPrim foldContainer = QC.property $ \(xs :: [(Int,Int)]) ->
  let p = E.fromList xs
      c = E.fromList xs
      -- we just need the function to be non-commutative
      func x y z = y - (2 * x) - (3 * z)
   in foldPrim func 42 p === foldContainer func 42 c

memberProp :: forall a t. (Arbitrary a, Show a) => ([a] -> t a) -> (a -> t a -> Bool) -> QC.Property
memberProp containerFromList containerMember = QC.property $ \(xs :: [a]) ->
  let c = containerFromList xs
   in all (\x -> containerMember x c) xs === True

setTripletonProp :: QC.Property
setTripletonProp = QC.property $ \(a :: Int16) (b :: Int16) (c :: Int16) ->
  SU.tripleton a b c === SU.fromList [a,b,c]

nonMemberProp :: forall t. ([Integer] -> t Integer) -> (Integer -> t Integer -> Bool) -> QC.Property
nonMemberProp containerFromList containerMember = QC.property $ \(xs :: [Integer]) ->
  let c = containerFromList xs
      upper = case xs of
        [] -> 42
        _ : _ -> maximum xs
      lower = case xs of
        [] -> (-42)
        _ : _ -> minimum xs
   in (containerMember (succ upper) c, containerMember (pred lower) c) === (False,False)

lookupProp :: forall k v t. (Arbitrary k, Show k, Ord k, Arbitrary v, Show v, Eq v) => ([(k,v)] -> t k v) -> (k -> t k v -> Maybe v) -> QC.Property
lookupProp containerFromList containerLookup = QC.property $ \(xs :: [(k,v)]) ->
  let ys = M.fromList xs
      c = containerFromList xs
   in all (\(x,_) -> containerLookup x c == M.lookup x ys) xs === True

lookupEmptyUnboxedLiftedMapProp :: QC.Property
lookupEmptyUnboxedLiftedMapProp = QC.property $ \(x :: Word16) ->
  MUL.lookup x (MUL.empty :: MUL.Map Word16 Integer) === Nothing

dietMemberProp :: forall a t. (Arbitrary a, Show a, Ord a, Arbitrary a, Show (t a)) => ([(a,a)] -> t a) -> (a -> t a -> Bool) -> QC.Property
dietMemberProp containerFromList containerLookup = QC.property $ \(xs :: [a]) ->
  let c = containerFromList (map (\a -> (a,a)) xs)
   in QC.counterexample ("original list: " ++ show xs ++ "; diet set: " ++ show c) (all (\x -> containerLookup x c == True) xs === True)

dietLookupPropA :: forall k v t. (Arbitrary k, Show k, Ord k, Arbitrary v, Show v, Eq v, Show (t k v)) => ([(k,k,v)] -> t k v) -> (k -> t k v -> Maybe v) -> QC.Property
dietLookupPropA containerFromList containerLookup = QC.property $ \(xs :: [(k,v)]) ->
  let ys = M.fromList xs
      c = containerFromList (map (\(k,v) -> (k,k,v)) xs)
   in QC.counterexample ("original list: " ++ show xs ++ "; diet map: " ++ show c) (all (\(x,_) -> containerLookup x c == M.lookup x ys) xs === True)

dbtsIntervalMapLookupProp :: QC.Property
dbtsIntervalMapLookupProp = QC.property $ \(xs :: [(Word8,Word8,Integer)]) (k :: Word8) ->
  let ys = MIDBTS.fromList Nothing (fmap (\(lo,hi,r) -> (lo,hi,Just r)) xs)
      expected = fmap (\(_,_,r) -> r) (F.find (\(lo,hi,_) -> lo <= k && k <= hi) xs)
   in expected === MIDBTS.lookup k ys

dietDoubletonProp :: QC.Property
dietDoubletonProp = QC.property $ \(loA :: Word8) (hiA :: Word8) (valA :: Int) (loB :: Word8) (hiB :: Word8) (valB :: Int) ->
  (hiA >= loA && hiB >= loB)
  ==>
  (simpleDoubletonToList loA hiA valA loB hiB valB === E.toList (DMLL.singleton loA hiA valA SG.<> DMLL.singleton loB hiB valB))

dietValidProp :: QC.Property
dietValidProp = QC.property $ \(xs :: DMLL.Map Word8 Int) ->
  True === validDietTriples (E.toList xs)

intersectsSet :: Ord a => S.Set a -> S.Set a -> Bool
intersectsSet s1 s2 =
  let s3 = s1 <> s2
  in if length s3 == length s1 + length s2
    then False
    else True

intersectsWorksProp :: QC.Property
intersectsWorksProp = QC.property $ \(xs :: S.Set Int) (ys :: S.Set Int) ->
  intersectsSet xs ys == SU.intersects (SU.fromList (S.toList xs)) (SU.fromList (S.toList ys))

simpleDoubletonToList :: (Ord k, Enum k, Semigroup v, Eq v) => k -> k -> v -> k -> k -> v -> [(k,k,v)]
simpleDoubletonToList key1A key2A valA key1B key2B valB =
  let loA = min key1A key2A
      hiA = max key1A key2A
      loB = min key1B key2B
      hiB = max key1B key2B
   in deduplicate $ case compare loA loB of
        LT -> case compare hiA loB of
          LT -> [(loA,hiA,valA),(loB,hiB,valB)]
          EQ -> case compare hiA hiB of
            LT -> [(loA,pred loB,valA),(loB,hiA,valA SG.<> valB),(succ hiA,hiB,valB)]
            EQ -> [(loA,pred loB,valA),(loB,hiA,valA SG.<> valB)]
            GT -> error "simpleDoubletonToList: invariant violated"
          GT -> case compare hiA hiB of
            LT -> [(loA,pred loB,valA),(loB,hiA,valA SG.<> valB),(succ hiA,hiB,valB)]
            EQ -> [(loA,pred loB,valA),(loB,hiA,valA SG.<> valB)]
            GT -> [(loA,pred loB,valA),(loB,hiB,valA SG.<> valB),(succ hiB,hiA,valA)]
        EQ -> case compare hiA hiB of
          LT -> [(loA,hiA,valA SG.<> valB),(succ hiA, hiB, valB)]
          GT -> [(loB,hiB,valA SG.<> valB),(succ hiB, hiA, valA)]
          EQ -> [(loA,hiA,valA SG.<> valB)]
        GT -> case compare hiB loA of
          LT -> [(loB,hiB,valB),(loA,hiA,valA)]
          EQ -> case compare hiB hiA of
            LT -> [(loB,pred loA,valB),(loA,hiB,valA SG.<> valB),(succ hiB,hiA,valA)]
            EQ -> [(loB,pred loA,valB),(loA,hiB,valA SG.<> valB)]
            GT -> error "simpleDoubletonToList: invariant violated"
          GT -> case compare hiB hiA of
            LT -> [(loB,pred loA,valB),(loA,hiB,valA SG.<> valB),(succ hiB,hiA,valA)]
            EQ -> [(loB,pred loA,valB),(loA,hiB,valA SG.<> valB)]
            GT -> [(loB,pred loA,valB),(loA,hiA,valA SG.<> valB),(succ hiA,hiB,valB)]

validDietTriples :: (Enum k,Eq k,Eq v) => [(k,k,v)] -> Bool
validDietTriples xs = deduplicate xs == xs

deduplicate :: (Enum k,Eq k, Eq v) => [(k,k,v)] -> [(k,k,v)]
deduplicate [] = []
deduplicate (x : xs) = F.toList (deduplicateNonEmpty (x :| xs))

deduplicateNonEmpty :: (Enum k, Eq k, Eq v) => NonEmpty (k,k,v) -> NonEmpty (k,k,v)
deduplicateNonEmpty ((lo,hi,v) :| xs) = case xs of
  y : ys -> case deduplicateNonEmpty (y :| ys) of
    (lo',hi',v') :| xs' -> if v == v' && pred lo' == hi
      then (lo,hi',v) :| xs'
      else (lo,hi,v) :| ((lo',hi',v') : xs')
  [] -> (lo,hi,v) :| []

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

instance (Arbitrary a, Prim a) => Arbitrary (PrimArray a) where
  arbitrary = fmap E.fromList QC.arbitrary

instance (Arbitrary a, Prim a, Ord a) => Arbitrary (SU.Set a) where
  arbitrary = fmap E.fromList QC.arbitrary

instance (Arbitrary a, PrimUnlifted a, Ord a) => Arbitrary (SUL.Set a) where
  arbitrary = fmap E.fromList QC.arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (SL.Set a) where
  arbitrary = fmap E.fromList QC.arbitrary

instance (Arbitrary k, Prim k, Ord k, Arbitrary v, Prim v) => Arbitrary (MUU.Map k v) where
  arbitrary = fmap E.fromList QC.arbitrary

instance (Arbitrary k, Prim k, Ord k, Arbitrary v) => Arbitrary (MUL.Map k v) where
  arbitrary = fmap E.fromList QC.arbitrary

instance (Arbitrary k, Ord k, Arbitrary v) => Arbitrary (MLL.Map k v) where
  arbitrary = fmap E.fromList QC.arbitrary

instance (Arbitrary k, Ord k, Enum k, Bounded k, Arbitrary v, Semigroup v, Eq v) => Arbitrary (DMLL.Map k v) where
  arbitrary = DMLL.fromListAppend <$> QC.vectorOf 10 arbitraryOrderedPairValue
  shrink x = map E.fromList (QC.shrink (E.toList x))

instance (Ord k, Enum k, Eq v, Bounded k, Arbitrary k, Arbitrary v) => Arbitrary (MIDBTS.Map k v) where
  arbitrary = liftA2 MIDBTS.fromList QC.arbitrary (QC.vectorOf 10 arbitraryOrderedPairValue)
    
instance (Arbitrary k, Ord k, Arbitrary v, Eq v, Semigroup v) => Arbitrary (MSL.Map k v) where
  arbitrary = do
    len <- QC.choose (0,4)
    xs <- QC.vectorOf len $ do
      n <- QC.choose (0,3)
      ys <- QC.vector n
      v <- QC.arbitrary
      return (SL.fromList ys, v)
    return (MSL.fromList xs)
  shrink x =
    [ MSL.fromList (drop 1 y)
    ]
    where y = MSL.toList x

instance (Arbitrary k, Prim k, Ord k, Enum k, Bounded k, Arbitrary v, Semigroup v, Eq v) => Arbitrary (DMUL.Map k v) where
  arbitrary = do
    sz <- QC.choose (0,10)
    k <- QC.arbitrary
    xs <- increasingOrderedPairsHelper sz k
    ys <- forM xs $ \(lo,hi) -> do
      v <- QC.arbitrary
      return (lo,hi,v)
    return (DMUL.fromListAppend ys)
  shrink x = map E.fromList (QC.shrink (E.toList x))
    
instance (Arbitrary a, Ord a, Enum a, Bounded a) => Arbitrary (DSL.Set a) where
  arbitrary = DSL.fromList <$> QC.vectorOf 7 arbitraryOrderedPair
  shrink x = map E.fromList (QC.shrink (E.toList x))

instance (Arbitrary a, Ord a, Enum a, Bounded a) => Arbitrary (DUSL.Set a) where
  arbitrary = do
    sz <- QC.choose (0,7)
    k <- QC.arbitrary
    foldMap (\(lo,hi) -> DUSL.singleton (Just lo) (Just hi)) <$> increasingOrderedPairsHelper sz k
    
increasingOrderedPairsHelper :: (Ord k, Enum k, Bounded k) => Int -> k -> Gen [(k,k)]
increasingOrderedPairsHelper n k = if n > 0
  then case atLeastTwoGreaterThan k of
    Nothing -> return []
    Just vals -> do
      lo <- QC.elements vals
      hi <- QC.elements (equalToOrGreaterThan lo)
      xs <- increasingOrderedPairsHelper (n - 1) hi
      return ((lo,hi) : xs)
  else return []

equalToOrGreaterThan :: (Ord a, Bounded a, Enum a) => a -> [a]
equalToOrGreaterThan a0 =
  let a1 = if a0 < maxBound then succ a0 else a0
      a2 = if a1 < maxBound then succ a1 else a1
      a3 = if a2 < maxBound then succ a2 else a2
   in [a0,a1,a2,a3]

atLeastTwoGreaterThan :: (Enum a, Bounded a, Ord a) => a -> Maybe [a]
atLeastTwoGreaterThan a0 = do
  if a0 < maxBound
    then
      let a1 = succ a0
       in if a1 < maxBound
            then
              let a2 = succ a1
                  a3 = if a2 < maxBound then succ a2 else a2
                  a4 = if a3 < maxBound then succ a3 else a3
               in Just [a2,a3,a4]
            else Nothing
    else Nothing

arbitraryOrderedPair :: (Ord k, Enum k, Bounded k, Arbitrary k) => Gen (k,k)
arbitraryOrderedPair = do
  a0 <- QC.arbitrary
  let a1 = if a0 < maxBound then succ a0 else a0
      a2 = if a1 < maxBound then succ a1 else a1
      a3 = if a2 < maxBound then succ a2 else a2
  a' <- QC.elements [a0,a1,a2,a3]
  return (a0,a')

arbitraryOrderedPairValue :: (Ord k, Enum k, Bounded k, Arbitrary k, Arbitrary v) => Gen (k,k,v)
arbitraryOrderedPairValue = do
  (lo,hi) <- arbitraryOrderedPair
  v <- QC.arbitrary
  return (lo,hi,v)

instance SG.Semigroup Word where
  w <> _ = w

instance SG.Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0
  mappend = (SG.<>)
  
instance SG.Semigroup Integer where
  (<>) = (+)

instance Monoid Integer where
  mempty = 0
  mappend = (SG.<>)

deriving instance Arbitrary a => Arbitrary (SG.First a)

-- This type interpret the lowest two bits of the Word8
-- as the Universe value. Doing this is unsafe, but if the
-- data constructor of a type like this is not exported, it
-- is possible to build safe interfaces on top of this.
newtype UnboxedKey u = UnboxedKey Word8
  deriving (Show,Prim,Eq,Ord)
