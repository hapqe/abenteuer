module Main where

import Backpack (demonstrateFill)
import Demonstration (demonstrateSort)
import Selection (selectionSort)
import System.Console.CmdArgs
import Tournament (tournamentSort)

data Abenteuer
  = Tournament {n :: Int}
  | Selection {n :: Int}
  | Backpack {chestSize :: Int}
  deriving (Show, Data, Typeable)

selection :: Abenteuer
selection = Selection {n = 50 &= help "The numbers of elements to be sorted"}

tournament :: Abenteuer
tournament = Tournament {n = 50 &= help "The numbers of elements to be sorted"}

backpack :: Abenteuer
backpack = Backpack {chestSize = 13 &= help "How many slots the chest has"}

main :: IO ()
main = runAlgorithm =<< cmdArgs (modes [selection, tournament, backpack])

runAlgorithm :: Abenteuer -> IO ()
runAlgorithm Selection {n} = demonstrateSort selectionSort n
runAlgorithm Tournament {n} = demonstrateSort tournamentSort n
runAlgorithm Backpack {chestSize} = demonstrateFill chestSize
