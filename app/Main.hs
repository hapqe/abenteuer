module Main where

import Backpack (demonstrateFill)
import Bubble (bubbleSort)
import Demonstration (demonstrateSort)
import Proxmap (proxmapSort)
import Selection (selectionSort)
import System.Console.CmdArgs
import Tournament (tournamentSort)

data Abenteuer
  = Tournament {n :: Int}
  | Selection {n :: Int}
  | Proxmap {n :: Int}
  | Bubble {n :: Int}
  | Backpack {chestSize :: Int}
  deriving (Show, Data, Typeable)

selection :: Abenteuer
selection = Selection {n = 50 &= help "The numbers of elements to be sorted"}

tournament :: Abenteuer
tournament = Tournament {n = 50 &= help "The numbers of elements to be sorted"}

bubble :: Abenteuer
bubble = Bubble {n = 50 &= help "The numbers of elements to be sorted"}

proxmap :: Abenteuer
proxmap = Proxmap {n = 50 &= help "The numbers of elements to be sorted"}

backpack :: Abenteuer
backpack = Backpack {chestSize = 13 &= help "How many slots the chest has"}

main :: IO ()
main = runAlgorithm =<< cmdArgs (modes [selection, tournament, backpack, proxmap, bubble])

runAlgorithm :: Abenteuer -> IO ()
runAlgorithm Selection {n} = demonstrateSort selectionSort n
runAlgorithm Tournament {n} = demonstrateSort tournamentSort n
runAlgorithm Proxmap {n} = demonstrateSort proxmapSort n
runAlgorithm Bubble {n} = demonstrateSort bubbleSort n
runAlgorithm Backpack {chestSize} = demonstrateFill chestSize
