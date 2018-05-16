{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -O2 #-}

import Gauge.Main
import System.Random (randoms,mkStdGen)
import Data.Foldable (foldMap)
import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import qualified GHC.Exts as E
import qualified Data.Set.Unboxed as DSU
import qualified Data.Set.Lifted as DSL
import qualified Data.Map.Unboxed.Unboxed as DMUU
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

main :: IO ()
main = defaultMain
  [ bgroup "Map"
    [ bgroup "lookup" 
      [ bench "primitive-unboxed-unboxed" $ whnf lookupAllUnboxed bigUnboxedMap
      , bench "containers-map" $ whnf lookupAllContainers bigContainersMap
      , bench "containers-intmap" $ whnf lookupAllIntContainers bigContainersIntMap
      ]
    , bgroup "fold"
      [ bench "primitive-unboxed-unboxed" $ whnf (DMUU.foldlWithKey' reduction 0) bigUnboxedMap
      , bench "containers-map" $ whnf (M.foldlWithKey' reduction 0) bigContainersMap
      ]
    ]
  , bgroup "Set"
    [ bgroup "lookup" 
      [ bench "primitive-unboxed" $ whnf lookupAllSetUnboxed bigUnboxedSet
      , bench "primitive-lifted" $ whnf lookupAllSetLifted bigLiftedSet
      ]
    , bgroup "fold"
      [ bench "primitive-unboxed-unboxed" $ whnf (DSU.foldl' (+) 0) bigUnboxedMap
      , bench "containers-set" $ whnf (S.foldl' (+) 0) bigContainersSet
      ]
    , bgroup "concat"
      [ bgroup "fold"
        [ bench "20" $ whnf (foldMap DSU.singleton) randomArray20
        , bench "200" $ whnf (foldMap DSU.singleton) randomArray200
        , bench "2000" $ whnf (foldMap DSU.singleton) randomArray2000
        ]
      , bgroup "fromList"
        [ bench "20" $ whnf (E.fromList :: [Word] -> DSU.Set Word) randomArray20
        , bench "200" $ whnf (E.fromList :: [Word] -> DSU.Set Word) randomArray200
        , bench "2000" $ whnf (E.fromList :: [Word] -> DSU.Set Word) randomArray2000
        ]
      , bgroup "fromAscList"
        [ bench "20" $ whnf (E.fromList :: [Word] -> DSU.Set Word) ascArray20
        , bench "200" $ whnf (E.fromList :: [Word] -> DSU.Set Word) ascArray200
        , bench "2000" $ whnf (E.fromList :: [Word] -> DSU.Set Word) ascArray2000
        ]
      ]
    ]
  ]

reduction :: Int -> Int -> Int -> Int
reduction x y z = x + y + z

bigNumber :: Int
bigNumber = 100000

bigContainersSet :: S.Set Int
bigContainersSet = E.fromList (map (\x -> x `mod` (bigNumber * 2)) (take bigNumber (randoms (mkStdGen 75843))))

bigUnboxedSet :: DSU.Set Int
bigUnboxedSet = E.fromList (map (\x -> x `mod` (bigNumber * 2)) (take bigNumber (randoms (mkStdGen 75843))))

bigLiftedSet :: DSL.Set Int
bigLiftedSet = E.fromList (map (\x -> x `mod` (bigNumber * 2)) (take bigNumber (randoms (mkStdGen 75843))))

bigUnboxedMap :: DMUU.Map Int Int
bigUnboxedMap = E.fromList (map (\x -> (x `mod` (bigNumber * 2),x)) (take bigNumber (randoms (mkStdGen 75843))))

bigContainersMap :: M.Map Int Int
bigContainersMap = M.fromList (map (\x -> (x `mod` (bigNumber * 2),x)) (take bigNumber (randoms (mkStdGen 75843))))

bigContainersIntMap :: IM.IntMap Int
bigContainersIntMap = IM.fromList (map (\x -> (x `mod` (bigNumber * 2),x)) (take bigNumber (randoms (mkStdGen 75843))))

lookupAllUnboxed :: DMUU.Map Int Int -> Int
lookupAllUnboxed m = go 0 0 where
  go !acc !n = if n < bigNumber
    then go (acc + fromMaybe 0 (DMUU.lookup n m)) (n + 1)
    else acc

lookupAllSetUnboxed :: DSU.Set Int -> Int
lookupAllSetUnboxed m = go 0 0 where
  go !acc !n = if n < bigNumber
    then go (acc + bool 2 3 (DSU.member n m)) (n + 1)
    else acc

lookupAllSetLifted :: DSL.Set Int -> Int
lookupAllSetLifted m = go 0 0 where
  go !acc !n = if n < bigNumber
    then go (acc + bool 2 3 (DSL.member n m)) (n + 1)
    else acc

lookupAllContainers :: M.Map Int Int -> Int
lookupAllContainers m = go 0 0 where
  go !acc !n = if n < bigNumber
    then go (acc + fromMaybe 0 (M.lookup n m)) (n + 1)
    else acc

lookupAllIntContainers :: IM.IntMap Int -> Int
lookupAllIntContainers m = go 0 0 where
  go !acc !n = if n < bigNumber
    then go (acc + fromMaybe 0 (IM.lookup n m)) (n + 1)
    else acc

ascArray20 :: [Word]
ascArray20 = take 20 (enumFrom 0)

ascArray200 :: [Word]
ascArray200 = take 200 (enumFrom 0)

ascArray2000 :: [Word]
ascArray2000 = take 2000 (enumFrom 0)

randomArray20 :: [Word]
randomArray20 = take 20 (randoms (mkStdGen 75843))

randomArray200 :: [Word]
randomArray200 = take 200 (randoms (mkStdGen 75843))

randomArray2000 :: [Word]
randomArray2000 = take 2000 (randoms (mkStdGen 75843))
