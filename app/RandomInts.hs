module RandomInts (getRandomIntsFast) where

import Data.List (unfoldr)
import System.Random

getRandomIntsFast :: Int -> (Int, Int) -> IO [Int]
getRandomIntsFast n range = do
  gen <- getStdGen -- Get the global random number generator
  let (gen1, gen2) = split gen -- Split the generator into two
      randomInts = take n $ unfoldr (Just . randomR range) gen1 -- Generate n random numbers
  setStdGen gen2 -- Update the global generator state
  return randomInts