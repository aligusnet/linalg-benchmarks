module Main where

import Criterion.Main (defaultMain, bgroup, bench, nf, nfIO, whnf)
import qualified Numeric.LinearAlgebra as LA


main :: IO ()
main = do
  !a <- LA.rand 1000 1000
  !b <- LA.rand 1000 1000
  !x <- LA.flatten <$> LA.rand 1000 1000
  !y <- LA.flatten <$> LA.rand 1000 1000
  let !ata = LA.mTm a
  
  defaultMain [
    bgroup "Linear Algebra" [
        bench "random matrix" $ nfIO (LA.rand 1000 1000)
        , bench "inv" $ nf LA.inv a
        , bench "Cholesky decomposition" $ nf LA.chol ata
        , bench "vector dot product" $ whnf (LA.dot x) y
        , bench "matrix transpose" $ nf LA.tr a
        , bench "matrix multiplication" $ nf (a LA.<>) b
        , bench "sigmoid" $ nf sigmoid a
        , bench "ReLU" $ nf relu a
        ]
    ]


sigmoid :: Floating a => a -> a
sigmoid z = 1 / (1+exp(-z))


relu :: LA.Matrix LA.R -> LA.Matrix LA.R
relu x = x * (LA.step x)
