{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Primitive
import Data.Primitive.UnliftedArray (PrimUnlifted)
import Data.Word
import Data.Proxy (Proxy(..))
import Data.Int

import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.QuickCheck (Arbitrary,Gen,(===),(==>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Control.Monad.ST (ST)
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
import qualified Data.Map.Unboxed.Unboxed as MUU
import qualified Data.Diet.Map.Unboxed.Lifted as DMUL
import qualified Data.Diet.Map.Lifted.Lifted as DMLL
import qualified Data.Diet.Set.Lifted as DSL

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
      , TQC.testProperty "foldr" (QCCL.foldrProp int32 SL.foldr)
      , TQC.testProperty "foldl'" (QCCL.foldlProp int16 SL.foldl')
      , TQC.testProperty "foldr'" (QCCL.foldrProp int32 SL.foldr')
      , TQC.testProperty "difference" differenceProp
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
        ]
      ]
    ]
  , testGroup "Diet"
    [ testGroup "Set"
      [ testGroup "Lifted"
        [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (DSL.Set Word16)))
        , lawsToTest (QCC.ordLaws (Proxy :: Proxy (DSL.Set Word16)))
        , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (DSL.Set Word16)))
        , lawsToTest (QCC.isListLaws (Proxy :: Proxy (DSL.Set Word16)))
        , TQC.testProperty "member" (dietMemberProp @Word8 E.fromList DSL.member)
        , TQC.testProperty "difference" dietSetDifferenceProp
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
      [ testGroup "Lifted"
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

dietSetDifferenceProp :: QC.Property
dietSetDifferenceProp = QC.property $ \(xs :: DSL.Set Word8) (ys :: DSL.Set Word8) ->
  let xs' = dietSetToSet xs
      ys' = dietSetToSet ys
   in DSL.difference xs ys === DSL.fromList (map (\x -> (x,x)) (S.toList (S.difference xs' ys')))

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
  (\lo hi s -> S.fromList (enumFromTo lo hi) <> s)
  mempty

differenceProp :: QC.Property
differenceProp = QC.property $ \(xs :: S.Set Word8) (ys :: S.Set Word8) ->
  let xs' = SL.fromList (S.toList xs)
      ys' = SL.fromList (S.toList ys)
   in SL.toList (SL.difference xs' ys') === S.toList (S.difference xs ys)

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

lookupProp :: forall k v t. (Arbitrary k, Show k, Ord k, Arbitrary v, Show v, Eq v) => ([(k,v)] -> t k v) -> (k -> t k v -> Maybe v) -> QC.Property
lookupProp containerFromList containerLookup = QC.property $ \(xs :: [(k,v)]) ->
  let ys = M.fromList xs
      c = containerFromList xs
   in all (\(x,_) -> containerLookup x c == M.lookup x ys) xs === True

dietMemberProp :: forall a t. (Arbitrary a, Show a, Ord a, Arbitrary a, Show (t a)) => ([(a,a)] -> t a) -> (a -> t a -> Bool) -> QC.Property
dietMemberProp containerFromList containerLookup = QC.property $ \(xs :: [a]) ->
  let c = containerFromList (map (\a -> (a,a)) xs)
   in QC.counterexample ("original list: " ++ show xs ++ "; diet set: " ++ show c) (all (\x -> containerLookup x c == True) xs === True)

dietLookupPropA :: forall k v t. (Arbitrary k, Show k, Ord k, Arbitrary v, Show v, Eq v, Show (t k v)) => ([(k,k,v)] -> t k v) -> (k -> t k v -> Maybe v) -> QC.Property
dietLookupPropA containerFromList containerLookup = QC.property $ \(xs :: [(k,v)]) ->
  let ys = M.fromList xs
      c = containerFromList (map (\(k,v) -> (k,k,v)) xs)
   in QC.counterexample ("original list: " ++ show xs ++ "; diet map: " ++ show c) (all (\(x,_) -> containerLookup x c == M.lookup x ys) xs === True)

dietDoubletonProp :: QC.Property
dietDoubletonProp = QC.property $ \(loA :: Word8) (hiA :: Word8) (valA :: Int) (loB :: Word8) (hiB :: Word8) (valB :: Int) ->
  (hiA >= loA && hiB >= loB)
  ==>
  (simpleDoubletonToList loA hiA valA loB hiB valB === E.toList (DMLL.singleton loA hiA valA <> DMLL.singleton loB hiB valB))

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

instance (Arbitrary k, Ord k, Enum k, Bounded k, Arbitrary v, Semigroup v, Eq v) => Arbitrary (DMLL.Map k v) where
  arbitrary = DMLL.fromListAppend <$> QC.vectorOf 10 arbitraryOrderedPairValue
  shrink x = map E.fromList (QC.shrink (E.toList x))
    
instance (Arbitrary k, Prim k, Ord k, Enum k, Bounded k, Arbitrary v, Semigroup v, Eq v) => Arbitrary (DMUL.Map k v) where
  arbitrary = DMUL.fromListAppend <$> QC.vectorOf 10 arbitraryOrderedPairValue
  shrink x = map E.fromList (QC.shrink (E.toList x))
    
instance (Arbitrary a, Ord a, Enum a, Bounded a) => Arbitrary (DSL.Set a) where
  arbitrary = DSL.fromList <$> QC.vectorOf 7 arbitraryOrderedPair
  shrink x = map E.fromList (QC.shrink (E.toList x))
    
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

