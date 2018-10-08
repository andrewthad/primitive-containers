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
import Data.Dependent.Map.Class (Universally(..),ApplyUniversally(..))
import Data.Exists (EqForeach(..),OrdForeach(..),EqForallPoly(..),OrdForallPoly(..),Sing)
import Data.Exists (FromJSONForeach(..),SemigroupForeach(..))
import Data.Exists (PrimForall(..),ToJSONKeyForall(..),ToJSONKeyFunctionForall(..))
import Data.Exists (ToJSONForeach(..),FromJSONKeyExists(..),Exists(..))
import Data.Exists (ToSing(..),DependentPair(..),ShowForall(..),ShowForeach(..))
import Data.Exists (WitnessedEquality(..),WitnessedOrdering(..),EqForall(..),OrdForall(..))
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Primitive.UnliftedArray (PrimUnlifted)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup)
import Test.HUnit.Base (assertEqual)
import Test.QuickCheck (Arbitrary,Gen,(===),(==>))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit (testCase,(@?=))
import Text.Read (readMaybe)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AEE
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
import qualified Data.Dependent.Map.Lifted.Lifted as DPMLL
import qualified Data.Dependent.Map.Unboxed.Lifted as DPMUL
import qualified Data.Map.Subset.Strict.Lifted as MSL
import qualified Data.Map.Interval.DBTS.Lifted.Lifted as MIDBTS

main :: IO ()
main = defaultMain $ testGroup "Data"
  [ testGroup "Set"
    [ testGroup "Unboxed"
      [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (SU.Set Int16)))
      , lawsToTest (QCC.ordLaws (Proxy :: Proxy (SU.Set Int16)))
      , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (SU.Set Int16)))
      , lawsToTest (QCC.isListLaws (Proxy :: Proxy (SU.Set Int16)))
      , TQC.testProperty "member" (memberProp @Int16 E.fromList SU.member)
      ]
    , testGroup "Lifted"
      [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (SL.Set Integer)))
      , lawsToTest (QCC.ordLaws (Proxy :: Proxy (SL.Set Integer)))
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
        , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (MLL.Map Integer Integer)))
        , lawsToTest (QCC.isListLaws (Proxy :: Proxy (MLL.Map Integer Integer)))
        , TQC.testProperty "appendWithKey" appendWithKeyLiftedLiftedProp
        ]
      ]
    , testGroup "Interval"
      [ testGroup "DBTS"
        [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (MIDBTS.Map Word8 Integer)))
        , lawsToTest (QCC.specialSemigroupLaws (Proxy :: Proxy (MIDBTS.Map Word8 (S.Set Integer))) [QCC.idempotent, QCC.commutative])
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
  , testGroup "Dependent"
    [ testGroup "Map"
      [ -- testGroup "Lifted"
        -- [ testGroup "Lifted"
        --   [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (DPMLL.Map Key Value)))
        --   , lawsToTest (QCC.ordLaws (Proxy :: Proxy (DPMLL.Map Key Value)))
        --   , lawsToTest (QCC.isListLaws (Proxy :: Proxy (DPMLL.Map Key Value)))
        --   , lawsToTest (QCC.semigroupLaws (Proxy :: Proxy (DPMLL.Map Key Value)))
        --   , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (DPMLL.Map Key Value)))
        --   ]
        -- ]
        testGroup "Unboxed"
        [ testGroup "Lifted"
          [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (DPMUL.Map UnboxedKey Value)))
          , lawsToTest (QCC.ordLaws (Proxy :: Proxy (DPMUL.Map UnboxedKey Value)))
          , lawsToTest (QCC.isListLaws (Proxy :: Proxy (DPMUL.Map UnboxedKey Value)))
          , lawsToTest (QCC.jsonLaws (Proxy :: Proxy (DPMUL.Map UnboxedKey Value)))
          , lawsToTest (QCC.semigroupLaws (Proxy :: Proxy (DPMUL.Map UnboxedKey Value)))
          , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (DPMUL.Map UnboxedKey Value)))
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
          , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (DUSL.Set Word8)))
          ]
        ]
      ]
    , testGroup "Set"
      [ testGroup "Lifted"
        [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (DSL.Set Word16)))
        , lawsToTest (QCC.ordLaws (Proxy :: Proxy (DSL.Set Word16)))
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

data Universe
  = UniverseInt
  | UniverseOrdering
  | UniverseBool
  | UniverseChar

data SingUniverse :: Universe -> Type where
  SingUniverseInt :: SingUniverse 'UniverseInt
  SingUniverseOrdering :: SingUniverse 'UniverseOrdering
  SingUniverseBool :: SingUniverse 'UniverseBool
  SingUniverseChar :: SingUniverse 'UniverseChar

deriving instance Show (SingUniverse u)

type instance Sing = SingUniverse

type family Interpret (u :: Universe) :: Type where
  Interpret 'UniverseInt = Integer
  Interpret 'UniverseOrdering = Ordering
  Interpret 'UniverseBool = Bool
  Interpret 'UniverseChar = Char

newtype Value :: Universe -> Type where
  Value :: Interpret u -> Value u

instance EqForeach Value where
  eqForeach SingUniverseInt (Value x) (Value y) = x == y
  eqForeach SingUniverseOrdering (Value x) (Value y) = x == y
  eqForeach SingUniverseBool (Value x) (Value y) = x == y
  eqForeach SingUniverseChar (Value x) (Value y) = x == y

instance OrdForeach Value where
  compareForeach SingUniverseInt (Value x) (Value y) = compare x y
  compareForeach SingUniverseOrdering (Value x) (Value y) = compare x y
  compareForeach SingUniverseBool (Value x) (Value y) = compare x y
  compareForeach SingUniverseChar (Value x) (Value y) = compare x y

instance ShowForeach Value where
  showsPrecForeach SingUniverseInt p (Value x) = showsPrec p x
  showsPrecForeach SingUniverseBool p (Value x) = showsPrec p x
  showsPrecForeach SingUniverseOrdering p (Value x) = showsPrec p x
  showsPrecForeach SingUniverseChar p (Value x) = showsPrec p x

instance SemigroupForeach Value where
  appendForeach SingUniverseInt (Value x) (Value y) = Value (x + y)
  appendForeach SingUniverseBool (Value x) (Value y) = Value (x && y)
  appendForeach SingUniverseOrdering (Value x) (Value y) = Value (x <> y)
  appendForeach SingUniverseChar (Value x) (Value _) = Value x

-- This type interpret the lowest two bits of the Word8
-- as the Universe value. Doing this is unsafe, but if the
-- data constructor of a type like this is not exported, it
-- is possible to build safe interfaces on top of this.
newtype UnboxedKey u = UnboxedKey Word8
  deriving (Show,Prim,Eq,Ord)

unboxedIntKey :: Word8 -> UnboxedKey 'UniverseInt
unboxedIntKey w = UnboxedKey (w * 4 + 0)

unboxedBoolKey :: Word8 -> UnboxedKey 'UniverseBool
unboxedBoolKey w = UnboxedKey (w * 4 + 1)

unboxedOrderingKey :: Word8 -> UnboxedKey 'UniverseOrdering
unboxedOrderingKey w = UnboxedKey (w * 4 + 2)

unboxedCharKey :: Word8 -> UnboxedKey 'UniverseChar
unboxedCharKey w = UnboxedKey (w * 4 + 3)

instance ToJSONKeyForall UnboxedKey where
  toJSONKeyForall = ToJSONKeyTextForall
    (\(UnboxedKey n) -> T.pack (show n))
    (\(UnboxedKey n) -> AEE.text (T.pack (show n)))

instance FromJSONKeyExists UnboxedKey where
  fromJSONKeyExists = AE.FromJSONKeyTextParser
    (\t -> case readMaybe (T.unpack t) of
      Nothing -> fail "Value, FromJSONKeyExists: bad value"
      Just w -> return (Exists (UnboxedKey w))
    )

instance FromJSONForeach Value where
  parseJSONForeach SingUniverseInt = fmap Value . AE.parseJSON 
  parseJSONForeach SingUniverseBool = fmap Value . AE.parseJSON
  parseJSONForeach SingUniverseOrdering = fmap Value . AE.parseJSON
  parseJSONForeach SingUniverseChar = fmap Value . AE.parseJSON

instance ToJSONForeach Value where
  toJSONForeach SingUniverseInt (Value a) = AE.toJSON a
  toJSONForeach SingUniverseBool (Value a) = AE.toJSON a
  toJSONForeach SingUniverseOrdering (Value a) = AE.toJSON a
  toJSONForeach SingUniverseChar (Value a) = AE.toJSON a

instance ToSing UnboxedKey where
  toSing (UnboxedKey w) = case mod w 4 of
    0 -> unsafeCoerce SingUniverseInt
    1 -> unsafeCoerce SingUniverseBool
    2 -> unsafeCoerce SingUniverseOrdering
    _ -> unsafeCoerce SingUniverseChar

instance ShowForall UnboxedKey where
  showsPrecForall = showsPrec

instance EqForall UnboxedKey where
  eqForall = (==)

instance EqForallPoly UnboxedKey where
  eqForallPoly (UnboxedKey a) (UnboxedKey b) = if a == b
    then unsafeCoerce WitnessedEqualityEqual
    else WitnessedEqualityUnequal

instance OrdForall UnboxedKey where
  compareForall = compare

instance OrdForallPoly UnboxedKey where
  compareForallPoly (UnboxedKey a) (UnboxedKey b) = case compare a b of
    LT -> WitnessedOrderingLT
    GT -> WitnessedOrderingGT
    EQ -> unsafeCoerce WitnessedOrderingEQ

data Key u = Key !Int !(SingUniverse u)
  deriving (Show)

instance ShowForall Key where
  showsPrecForall = showsPrec

instance ToSing Key where
  toSing (Key _ s) = s

instance EqForall Key where
  eqForall (Key i1 _) (Key i2 _) = i1 == i2

instance OrdForall Key where
  compareForall (Key i1 _) (Key i2 _) = compare i1 i2

instance EqForallPoly Key where
  eqForallPoly (Key i1 s1) (Key i2 s2) = if i1 == i2
    then case s1 of
      SingUniverseInt -> case s2 of
        SingUniverseInt -> WitnessedEqualityEqual
        _ -> WitnessedEqualityUnequal
      SingUniverseOrdering -> case s2 of
        SingUniverseOrdering -> WitnessedEqualityEqual
        _ -> WitnessedEqualityUnequal
      SingUniverseBool -> case s2 of
        SingUniverseBool -> WitnessedEqualityEqual
        _ -> WitnessedEqualityUnequal
      SingUniverseChar -> case s2 of
        SingUniverseChar -> WitnessedEqualityEqual
        _ -> WitnessedEqualityUnequal
    else WitnessedEqualityUnequal

instance EqForall SingUniverse where
  eqForall _ _ = True

instance OrdForall SingUniverse where
  compareForall _ _ = EQ

instance EqForallPoly SingUniverse where
  eqForallPoly SingUniverseInt SingUniverseInt = WitnessedEqualityEqual
  eqForallPoly SingUniverseInt _ = WitnessedEqualityUnequal
  eqForallPoly SingUniverseBool SingUniverseBool = WitnessedEqualityEqual
  eqForallPoly SingUniverseBool _ = WitnessedEqualityUnequal
  eqForallPoly SingUniverseOrdering SingUniverseOrdering = WitnessedEqualityEqual
  eqForallPoly SingUniverseOrdering _ = WitnessedEqualityUnequal
  eqForallPoly SingUniverseChar SingUniverseChar = WitnessedEqualityEqual
  eqForallPoly SingUniverseChar _ = WitnessedEqualityUnequal

instance OrdForallPoly SingUniverse where
  compareForallPoly SingUniverseInt SingUniverseInt      = WitnessedOrderingEQ
  compareForallPoly SingUniverseInt SingUniverseOrdering = WitnessedOrderingLT
  compareForallPoly SingUniverseInt SingUniverseBool     = WitnessedOrderingLT
  compareForallPoly SingUniverseInt SingUniverseChar     = WitnessedOrderingLT
  compareForallPoly SingUniverseOrdering SingUniverseInt      = WitnessedOrderingGT
  compareForallPoly SingUniverseOrdering SingUniverseOrdering = WitnessedOrderingEQ
  compareForallPoly SingUniverseOrdering SingUniverseBool     = WitnessedOrderingLT
  compareForallPoly SingUniverseOrdering SingUniverseChar     = WitnessedOrderingLT
  compareForallPoly SingUniverseBool SingUniverseInt      = WitnessedOrderingGT
  compareForallPoly SingUniverseBool SingUniverseOrdering = WitnessedOrderingGT
  compareForallPoly SingUniverseBool SingUniverseBool     = WitnessedOrderingEQ
  compareForallPoly SingUniverseBool SingUniverseChar     = WitnessedOrderingLT
  compareForallPoly SingUniverseChar SingUniverseInt      = WitnessedOrderingGT
  compareForallPoly SingUniverseChar SingUniverseOrdering = WitnessedOrderingGT
  compareForallPoly SingUniverseChar SingUniverseBool     = WitnessedOrderingGT
  compareForallPoly SingUniverseChar SingUniverseChar     = WitnessedOrderingEQ

instance OrdForallPoly Key where
  compareForallPoly (Key i1 s1) (Key i2 s2) = case compare i1 i2 of
    LT -> WitnessedOrderingLT
    GT -> WitnessedOrderingGT
    EQ -> compareForallPoly s1 s2

class ArbitraryDependentPair k v where
  arbitraryDependentPair :: Gen (DependentPair k v)

instance ArbitraryDependentPair k v => Arbitrary (DependentPair k v) where
  arbitrary = arbitraryDependentPair

instance ArbitraryDependentPair Key Value where
  arbitraryDependentPair = do
    (i :: Int) <- QC.choose (0, 10)
    QC.oneof
      [ DependentPair (Key i SingUniverseInt) . Value <$> QC.arbitrary
      , DependentPair (Key i SingUniverseBool) . Value <$> QC.arbitrary
      , DependentPair (Key i SingUniverseChar) . Value <$> QC.arbitrary
      , DependentPair (Key i SingUniverseOrdering) . Value <$> QC.arbitrary
      ]

instance ArbitraryDependentPair UnboxedKey Value where
  arbitraryDependentPair = do
    (i :: Word8) <- QC.choose (0, 10)
    QC.oneof
      [ DependentPair (unboxedIntKey i) . Value <$> QC.arbitrary
      , DependentPair (unboxedBoolKey i) . Value <$> QC.arbitrary
      , DependentPair (unboxedCharKey i) . Value <$> QC.arbitrary
      , DependentPair (unboxedOrderingKey i) . Value <$> QC.arbitrary
      ]
    
instance (ArbitraryDependentPair k v, OrdForallPoly k) => Arbitrary (DPMLL.Map k v) where
  arbitrary = do
    len <- QC.choose (0,35)
    DPMLL.fromList <$> QC.vectorOf len arbitraryDependentPair

instance (ArbitraryDependentPair k v, OrdForallPoly k, Universally k Prim, ApplyUniversally k Prim) => Arbitrary (DPMUL.Map k v) where
  arbitrary = do
    len <- QC.choose (0,35)
    DPMUL.fromList <$> QC.vectorOf len arbitraryDependentPair

instance Universally UnboxedKey Prim where
  universally _ _ _ x = x

instance ApplyUniversally UnboxedKey Prim where
  applyUniversallyLifted _ _ _ x = x
  applyUniversallyUnlifted _ _ _ x = x

-- very unsafe instance
instance PrimForall UnboxedKey where
  sizeOfForall# _ = sizeOf# (undefined :: UnboxedKey a)
  alignmentForall# _ = alignment# (undefined :: UnboxedKey a)
  indexByteArrayForall# = indexByteArray#
  readByteArrayForall# = readByteArray#
  writeByteArrayForall# = writeByteArray#
  setByteArrayForall# = setByteArray#
  readOffAddrForall# = readOffAddr#
  writeOffAddrForall# = writeOffAddr#
  indexOffAddrForall# = indexOffAddr#
  setOffAddrForall# = setOffAddr#

