{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , fromListN
  , fromList
  , toList
  ) where

import Prelude hiding (compare,showsPrec,lookup,map)
import qualified Prelude as P

import Control.Applicative (liftA2)
import Control.Monad.ST (ST,runST)
import Data.Bifunctor (second)
import Data.Semigroup (Semigroup)
import qualified Key as K
import qualified Value as V
import qualified Data.Semigroup as SG
import qualified GHC.Exts as E

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

fromListN :: (K.Ctx k, Ord k, V.Ctx v) => Int -> [(k,v)] -> Map k v
fromListN _ = fromList

fromList :: (K.Ctx k, Ord k, V.Ctx v) => [(k,v)] -> Map k v
fromList = foldr (\(k,v) acc -> appendWith (\_ a -> a) (singleton k v) acc) empty

fromListAppend :: (K.Ctx k, Ord k, V.Ctx v, Semigroup v) => [(k,v)] -> Map k v
fromListAppend = foldr (\(k,v) acc -> append (singleton k v) acc) empty

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
unionArrWith combine keysA valsA keysB valsB = runST $ do
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
