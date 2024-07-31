module Proxmap (proxmapSort) where

import Data.Array (Array, elems, listArray, (!), (//))
import Data.Maybe (catMaybes)
import Statistics.Distribution (Distribution (cumulative))
import Statistics.Distribution.Normal (NormalDistribution, normalDistr)

dist :: NormalDistribution
dist = normalDistr 0 1

overshoot :: Double
overshoot = 0.25

proxPos :: [Double] -> Double -> Int
proxPos xs x = round $ cumulative dist x * proxLenght xs

proxLenght :: [Double] -> Double
proxLenght xs = fromIntegral (length xs) * (1.0 + overshoot)

proxInsert :: (Ord a) => Int -> a -> Array Int (Maybe a) -> Array Int (Maybe a)
proxInsert approx x xs = case xs ! approx of
  Nothing -> xs // [(approx, Just x)]
  Just y -> if x < y then proxInsert (approx + 1) x xs else let ys = xs // [(approx, Just x)] in proxInsert (approx + 1) y ys

-- | A terrible proxmap algorithm!
proxmapSort :: [Double] -> [Double]
proxmapSort xs = catMaybes $ elems $ foldl (\acc x -> proxInsert (proxPos xs x) x acc) initial xs
  where
    count = round $ proxLenght xs + 10 -- since the elements get to get pushed to the right
    initial = listArray (0, count - 1) $ replicate count Nothing
