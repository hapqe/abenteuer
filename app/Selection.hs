module Selection (selectionSort) where

import Prelude hiding (min)

selectionSort :: [Int] -> [Int]
selectionSort [] = []
selectionSort xs = let x = maximum xs in selectionSort (remove x xs) ++ [x]
  where
    remove _ [] = []
    remove a (y : ys)
      | y == a = ys
      | otherwise = y : remove a ys
