{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Monoid
import Data.Primitive
import Data.Primitive.Array
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Primitive.SmallArray
import Data.Primitive.PrimArray
import Data.Word
import Data.Proxy (Proxy(..))
import Data.Function (on)
import Data.Int

import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.QuickCheck (Arbitrary,Arbitrary1,Gen,(===))
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Classes as QCC
import qualified Test.QuickCheck.Classes.IsList as QCCL
import qualified Data.List as L
import qualified GHC.Exts as E

import qualified Data.Set.Unboxed as DSU
import qualified Data.Set.Lifted as DSL

main :: IO ()
main = defaultMain $ testGroup "properties"
  [ testGroup "Data"
    [ testGroup "Set"
      [ testGroup "Unboxed"
        [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (DSU.Set Int16)))
        , lawsToTest (QCC.ordLaws (Proxy :: Proxy (DSU.Set Int16)))
        , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (DSU.Set Int16)))
        , lawsToTest (QCC.isListLaws (Proxy :: Proxy (DSU.Set Int16)))
        , TQC.testProperty "member" (memberProp @Int16 E.fromList DSU.member)
        ]
      , testGroup "Lifted"
        [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (DSL.Set Integer)))
        , lawsToTest (QCC.ordLaws (Proxy :: Proxy (DSL.Set Integer)))
        , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (DSL.Set Integer)))
        , lawsToTest (QCC.isListLaws (Proxy :: Proxy (DSL.Set Int16)))
        , TQC.testProperty "member" (memberProp @Integer E.fromList DSL.member)
        ]
      ]
    ]
  ]

memberProp :: forall a t. (Arbitrary a, Show a) => ([a] -> t a) -> (a -> t a -> Bool) -> QC.Property
memberProp containerFromList containerMember = QC.property $ \(xs :: [a]) ->
  let c = containerFromList xs
   in all (\x -> containerMember x c) xs === True

byteArrayEqProp :: QC.Property
byteArrayEqProp = QC.property $ \(xs :: [Word8]) (ys :: [Word8]) ->
  (compareLengthFirst xs ys == EQ) === (byteArrayFromList xs == byteArrayFromList ys)

compareLengthFirst :: [Word8] -> [Word8] -> Ordering
compareLengthFirst xs ys = (compare `on` length) xs ys <> compare xs ys

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

instance (Arbitrary a, Prim a, Ord a) => Arbitrary (DSU.Set a) where
  arbitrary = fmap fromList QC.arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (DSL.Set a) where
  arbitrary = fmap fromList QC.arbitrary
