module UnlimitedGameOfLife where

import Data.Function (on)
import Data.List (groupBy, sort, splitAt)

type Pos = (Int, Int)

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration world gen = case gen of
  0 -> world
  n -> getGeneration (construct (length world) posInfo) (gen - 1)
    where posInfo = [ ((row, col), nextRound (col,row) world) | row <-  [0 .. length world - 1], col <- [0 .. length (head world) - 1] ]

nextRound :: Pos -> [[Int]] -> Int
nextRound (x,y) world = let fate = sum $ map (\(a, b) -> world !! b !! a) $ neighbors (x, y) world in
  case fate of
    n | n < 2 || n > 3 -> 0
    n | n == 3 && world !! y !! x == 0 -> 1
    _ -> world !! y !! x

neighbors :: Pos -> [[Int]] -> [Pos]
neighbors (x, y) world = filter (\(a, b) -> onMap a b && (a,b)/=(x,y)) [(dx + x, dy + y) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
  where onMap x y = x `elem` [0 .. length (head world) - 1] && y  `elem` [0 .. length world - 1]


construct :: Int -> [(Pos, Int)] -> [[Int]]
construct rowLength world = splitEvery rowLength [ life | (pos, life) <- sort world]

splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list

htmlize :: [[Int]] -> String
htmlize = concatMap ((++ "<br>") . concatMap cell)
  where cell n = if n > 0 then "&#x2593;&#x2593;" else "&#x2591;&#x2591;"
