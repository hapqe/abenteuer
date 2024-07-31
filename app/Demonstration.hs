module Demonstration (demonstrateSort) where

import Control.DeepSeq
import Data.Time (diffUTCTime, getCurrentTime)
import RandomInts (getRandomIntsFast)

demonstrateSort :: ([Int] -> [Int]) -> Int -> IO ()
demonstrateSort alg n = do
  input <- getRandomIntsFast n (1, 10000)
  start <- getCurrentTime
  let sorted = alg input
  -- let sorted = sort input
  end <- sorted `deepseq` getCurrentTime
  print $ "Sorted " ++ show (length sorted) ++ " elements in " ++ show (diffUTCTime end start)
