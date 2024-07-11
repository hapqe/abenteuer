module Backpack (demonstrateFill) where

import Data.List (unfoldr, uncons)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)

type Chest = String

exampleTreasures :: [Treasure]
exampleTreasures =
    [ Treasure "🟫🟫" 7
    , Treasure "🟩🟩🟩" 11
    , Treasure "🟥🟥🟥🟥" 16
    , Treasure "🟦🟦🟦🟦🟦" 24
    , Treasure "🟧🟧🟧🟧🟧🟧🟧" 32
    , Treasure "🟪🟪🟪🟪🟪🟪🟪🟪" 36
    , Treasure "🟨🟨🟨🟨🟨🟨🟨🟨🟨" 43
    ]


emptyChests :: Int -> [Chest]
emptyChests size = map (`replicate` '⬜') [0 .. size]

data Treasure = Treasure {
    item :: String,
    value :: Int
} deriving Show

printChests :: [Chest] -> IO ()
printChests = mapM_ putStrLn


expect :: Maybe a -> [Char] -> a
expect x msg = fromMaybe (error msg) x

fillSmallest :: Treasure -> [Chest] -> [Chest]
fillSmallest smallest = map (concatMap (\x -> if length x == length (item smallest) then item smallest else x) . chunks (length $ item smallest))

-- from "https://stackoverflow.com/a/12882583"
chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)


chestValue :: [Treasure] -> Chest -> Int
chestValue treasures chest =
    let treasureValue t = (fst . flip expect "a treasure has to be non-empty!" . uncons $ item t, (fromIntegral $ value t :: Float) / fromIntegral (length $ item t))
        values = Map.fromList (('⬜', 0) : map treasureValue treasures)
    in round $ sum $ map (fromJust . flip Map.lookup values) chest

optimizeTreasure :: [Treasure] -> [Chest] -> Treasure -> [Chest]
optimizeTreasure treasures chests treasure =
    let maxValue a b = if chestValue treasures a > chestValue treasures b then a else b
        split = splitAt (length $ item treasure) chests
        check (a:as, b:bs) = a : optimizeTreasure treasures (as ++ maxValue (a ++ item treasure) b : bs) treasure
        check (_, _) = chests
    in check split

fillTreasures :: [Treasure] -> Int -> [Chest]
fillTreasures [] chestSize = emptyChests chestSize
fillTreasures treasures@(smallest : other) chestSize = foldl (optimizeTreasure treasures) (fillSmallest smallest $ emptyChests chestSize) other

demonstrateFill :: Int -> IO ()
demonstrateFill n = printChests $ fillTreasures exampleTreasures n