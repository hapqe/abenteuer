module Bubble where

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = foldl (const . swap) xs xs
  where
    swap [] = []
    swap [x] = [x]
    swap (x : y : ys) = if x < y then x : swap (y : ys) else y : swap (x : ys)
