module Selection (demonstrateSelection) where

import Control.DeepSeq (deepseq)
import Data.Time (getCurrentTime)
import Data.Time.Clock (diffUTCTime)
import RandomInts (getRandomIntsFast)
import Prelude hiding (min)

selectionSort :: [Int] -> [Int]
selectionSort [] = []
selectionSort xs = let x = maximum xs in selectionSort (remove x xs) ++ [x]
  where
    remove _ [] = []
    remove a (y : ys)
      | y == a = ys
      | otherwise = y : remove a ys

demonstrateSelection :: Int -> IO ()
demonstrateSelection n = do
  input <- getRandomIntsFast n (1, 10000)
  start <- getCurrentTime
  let sorted = selectionSort input
  -- let sorted = sort input
  end <- sorted `deepseq` getCurrentTime
  print $ "Sorted " ++ show (length sorted) ++ " elements in " ++ show (diffUTCTime end start)
