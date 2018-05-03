{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -O2 #-}

import Gauge.Main
import System.Random (randoms,mkStdGen)
import Data.Foldable (foldMap)
import Data.Maybe (fromMaybe)
import qualified GHC.Exts as E
import qualified Data.Set.Unboxed as DSU
import qualified Data.Map.Unboxed.Unboxed as DMUU
import qualified Data.Map.Strict as M

main :: IO ()
main = defaultMain
  [ bgroup "Map"
    [ bgroup "lookup" 
      [ bench "unboxed" $ whnf lookupAllUnboxed bigUnboxedMap
      , bench "containers" $ whnf lookupAllContainers bigContainersMap
      ]
    ]
  , bgroup "Set"
    [ bgroup "concat"
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

bigNumber :: Int
bigNumber = 100000

bigUnboxedMap :: DMUU.Map Int Int
bigUnboxedMap = E.fromList (map (\x -> (x `mod` (bigNumber * 2),x)) (take bigNumber (randoms (mkStdGen 75843))))

bigContainersMap :: M.Map Int Int
bigContainersMap = M.fromList (map (\x -> (x `mod` (bigNumber * 2),x)) (take bigNumber (randoms (mkStdGen 75843))))

lookupAllUnboxed :: DMUU.Map Int Int -> Int
lookupAllUnboxed m = go 0 0 where
  go !acc !n = if n < bigNumber
    then go (acc + fromMaybe 0 (DMUU.lookup n m)) (n + 1)
    else acc

lookupAllContainers :: M.Map Int Int -> Int
lookupAllContainers m = go 0 0 where
  go !acc !n = if n < bigNumber
    then go (acc + fromMaybe 0 (M.lookup n m)) (n + 1)
    else acc

ascArray20 :: [Word]
ascArray20 = take 20 (enumFrom 0)

ascArray200 :: [Word]
ascArray200 = take 200 (enumFrom 0)

ascArray2000 :: [Word]
ascArray2000 = take 2000 (enumFrom 0)

ascArray20000:: [Word]
ascArray20000 = take 20000 (enumFrom 0)

randomArray20 :: [Word]
randomArray20 = take 20 (randoms (mkStdGen 75843))

randomArray200 :: [Word]
randomArray200 = take 200 (randoms (mkStdGen 75843))

randomArray2000 :: [Word]
randomArray2000 = take 2000 (randoms (mkStdGen 75843))
