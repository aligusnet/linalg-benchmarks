module Main where

import Criterion.Main (defaultMain, bgroup, bench, nf, nfIO, whnf)
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.Morpheus as M


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
        , bench "sum by rows" $ nf (reduceByRows V.sum) a
        , bench "sum by columns" $ nf (reduceByColumns V.sum) a
        , bench "max index in rows" $ nf (reduceByRowsV (fromIntegral . LA.maxIndex) ) a
        , bench "max index in columns" $ nf (reduceByColumnsV (fromIntegral . LA.maxIndex) ) a
        , bench "sigmoid (morpheus)" $ nf M.sigmoid a
        , bench "sigmoid gradient (morpheus)" $ nf M.sigmoidGradient a
        , bench "sum by rows (morpheus)" $ nf M.rowSum a
        , bench "sum by columns (morpheus)" $ nf M.columnSum a
        , bench "max index in rows (morpheus)" $ nf M.rowMaxIndex a
        , bench "max index in columns (morpheus)" $ nf M.columnMaxIndex a

        ]
    ]


type Matrix = LA.Matrix LA.R
type Vector = LA.Vector LA.R
type R = LA.R

sigmoid :: Floating a => a -> a
sigmoid z = 1 / (1+exp(-z))


relu :: Matrix -> Matrix
relu x = x * (LA.step x)


reduceByRowsV :: (Vector -> R) -> Matrix -> Vector
reduceByRowsV f = LA.vector . map f . LA.toRows


reduceByColumnsV :: (Vector -> R) -> Matrix -> Vector
reduceByColumnsV f = LA.vector . map f . LA.toColumns


reduceByRows :: (Vector -> R) -> Matrix -> Matrix
reduceByRows f = LA.asColumn . reduceByRowsV f


reduceByColumns :: (Vector -> R) -> Matrix -> Matrix
reduceByColumns f = LA.asRow . reduceByColumnsV f
