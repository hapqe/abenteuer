module Main where

import Backpack (demonstrateFill)
import Selection (demonstrateSelection)
import System.Console.CmdArgs
import Tournament (demonstrateTournament)

data Abenteuer
  = Tournament {n :: Int}
  | Selection {n :: Int}
  | Backpack {chestSize :: Int}
  deriving (Show, Data, Typeable)

tournament :: Abenteuer
tournament = Tournament {n = 50 &= help "The numbers of elements to be sorted"}

backpack :: Abenteuer
backpack = Backpack {chestSize = 13 &= help "How many slots the chest has"}

main :: IO ()
main = runAlgorithm =<< cmdArgs (modes [tournament, backpack])

runAlgorithm :: Abenteuer -> IO ()
runAlgorithm Tournament {n} = demonstrateTournament n
runAlgorithm Selection {n} = demonstrateSelection n
runAlgorithm Backpack {chestSize} = demonstrateFill chestSize
