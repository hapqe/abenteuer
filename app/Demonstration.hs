{-# LANGUAGE MonoLocalBinds #-}

module Demonstration (demonstrateSort, randomData) where

import Control.DeepSeq
import Control.Monad (replicateM)
import Data.Time (diffUTCTime, getCurrentTime)
import System.Random.MWC
import System.Random.MWC.Distributions

randomData :: GenIO -> Int -> IO [Double]
randomData gen n = replicateM n (normal 0 1 gen)

demonstrateSort :: ([Double] -> [Double]) -> Int -> IO ()
demonstrateSort alg n = do
  gen <- createSystemRandom
  input <- randomData gen n
  start <- getCurrentTime
  let sorted = alg input
  -- let sorted = sort input
  end <- sorted `deepseq` getCurrentTime
  print $ "Sorted " ++ show (length sorted) ++ " elements in " ++ show (diffUTCTime end start)
