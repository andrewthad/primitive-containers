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

import Data.Continuous.Set.Lifted (Inclusivity(..))
import Data.Functor.Const (Const(..))
import Data.Primitive.Unlifted.Class (PrimUnlifted)
import Data.Proxy (Proxy(..))
import Test.HUnit.Base (assertEqual)
import Test.QuickCheck (Arbitrary,(===))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit (testCase,(@?=))

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
import qualified Data.Continuous.Set.Lifted as CSL
import qualified Data.Map.Subset.Strict.Lifted as MSL

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
  ]

int16 :: Proxy Int16
int16 = Proxy

int32 :: Proxy Int32
int32 = Proxy

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

intersectsSet :: Ord a => S.Set a -> S.Set a -> Bool
intersectsSet s1 s2 =
  let s3 = s1 <> s2
  in if length s3 == length s1 + length s2
    then False
    else True

intersectsWorksProp :: QC.Property
intersectsWorksProp = QC.property $ \(xs :: S.Set Int) (ys :: S.Set Int) ->
  intersectsSet xs ys == SU.intersects (SU.fromList (S.toList xs)) (SU.fromList (S.toList ys))

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
