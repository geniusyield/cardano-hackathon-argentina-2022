{-# OPTIONS -Wno-unused-top-binds #-}

module GeniusYield.Simulator.Generator (
  randAmount,
  genSamplesFile,
  genPercentage,
  randTokenAUPrice,
  randTokenQ,
) where

import Control.Monad (replicateM)
import Data.Random (MonadRandom, sample)
import Data.Random.Distribution.Pareto (pareto)
import System.Random (randomRIO)

randTokenAUPrice :: IO Double
randTokenAUPrice = do
  x <- randomRIO (0 :: Double, 1)
  let k = 2 -- Mean
      a = 5 -- Value spread ∝ 1/a
  if x > 0.5
    then filteredPareto k a
    else invPareto k a

randAmount :: IO Double
randAmount = do
  x <- randomRIO (0 :: Double, 1)
  if x > 0.5
    then filteredPareto 100 10
    else invPareto 100 10

filteredPareto :: MonadRandom m => Double -> Double -> m Double
filteredPareto c k = go
  where
    origP = pareto c (k :: Double)
    go = do
      s <- sample origP
      if s < (2 * c) then return s else go

invPareto :: MonadRandom f => Double -> Double -> f Double
invPareto c k = inv <$> filteredPareto c k
  where
    inv p = c - (p - c)

genSamplesFile :: IO ()
genSamplesFile = do
  uP <- replicateM 2000 randTokenAUPrice
  writeFile "pareto_orig_and_inv_filt.csv" $ show uP

randTokenQ :: IO Int
randTokenQ = randomRIO (4, 1000)

genPercentage :: IO Double
genPercentage = round' <$> randomRIO (0.3, 1)
  where
    round' :: Double -> Double
    round' num = (fromIntegral . (round :: Double -> Int) $ num * f) / f
      where
        decimals = 4 :: Int
        f = 10 ^ decimals
