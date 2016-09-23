module UnlimitedGameOfLife where

import Data.Function (on)
import Data.List (groupBy, sort, splitAt, transpose)
-- import Debug.Trace

type Pos = (Int, Int)

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration world gen = case gen of
  0 -> world
  n -> getGeneration newWorld (gen - 1)
    where
      -- w = trace ("round n = " ++ show n ++ " new world = " ++ show newWorld ) newWorld
      newWorld = construct (length (head expandedWorld)) posInfo
      posInfo = [ ((row, col), nextRound (col,row) expandedWorld) | row <-  [0 .. length expandedWorld - 1], col <- [0 .. length (head expandedWorld) - 1] ]
      expandedWorld = expand world

nextRound :: Pos -> [[Int]] -> Int
nextRound (x,y) world = let fate = sum $ map (\(a, b) -> world !! b !! a) $ neighbors (x, y) world
                            current = world !! y !! x
                          in
                          case fate of
                            n | n < 2 || n > 3 -> 0
                            n | n == 3 && current == 0 -> 1
                            _ -> current

expand :: [[Int]] -> [[Int]]
expand [] = []
expand world = transpose $ (transpose . transpose) [sideBar] ++ transpose ([topBar] ++ world ++ [topBar]) ++ [sideBar]
  where topBar = [0 | _ <- [0 .. length (head world) - 1]]
        sideBar = [0 | _ <- [0 .. length world + 1]]

compact :: [[Int]] -> [[Int]]
compact [] = [[]]
compact a = (transpose . transpose . compact' .compact') a
  where compact' = transpose . reverse . dropWhile (\w -> 0 == sum w) . reverse . dropWhile (\w -> 0 == sum w)

neighbors :: Pos -> [[Int]] -> [Pos]
neighbors (x, y) world = filter  (\(a, b) -> onMap a b && (a,b)/=(x,y)) [(dx + x, dy + y) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
  where onMap x y = x `elem` [0 .. length (head world) - 1] && y  `elem` [0 .. length world - 1]


construct :: Int -> [(Pos, Int)] -> [[Int]]
construct rowLength world = compact $ splitEvery rowLength [ life | (pos, life) <- sort world]

splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list

htmlize :: [[Int]] -> String
htmlize = concatMap ((++ "<br>") . concatMap cell)
  where cell n = if n > 0 then "&#x2593;&#x2593;" else "&#x2591;&#x2591;"
