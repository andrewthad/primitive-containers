import Gauge.Main
import System.Random (randoms,mkStdGen)
import Data.Foldable (foldMap)
import qualified GHC.Exts as E
import qualified Data.Set.Unboxed as DSU

main :: IO ()
main = defaultMain
  [ bgroup "Set"
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
      ]
    ]
  ]

randomArray20 :: [Word]
randomArray20 = take 20 (randoms (mkStdGen 75843))

randomArray200 :: [Word]
randomArray200 = take 200 (randoms (mkStdGen 75843))

randomArray2000 :: [Word]
randomArray2000 = take 2000 (randoms (mkStdGen 75843))
