module UnlimitedGameOfLife where

import           Data.List (sort, transpose)

type Pos = (Int, Int)

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration world 0 = world
getGeneration world gen = getGeneration newWorld (gen - 1)
    where
      -- w = trace ("round n = " ++ show n ++ " new world = " ++ show newWorld ) newWorld
      newWorld = construct (length (head expandedWorld)) posInfo
      posInfo = [ ((row, col), nextRound (col,row) expandedWorld) | row <-  rows, col <- cols]
      rows = [0 .. length expandedWorld - 1]
      cols = [0 .. length (head expandedWorld) - 1]
      expandedWorld = expand world

nextRound :: Pos -> [[Int]] -> Int
nextRound (x,y) world = case fate of
                          n | n < 2 || n > 3 -> 0
                          n | n == 3 && current == 0 -> 1
                          _ -> current
                        where
                          fate = sum $ map (\(a, b) -> world !! b !! a) $ neighbors (x, y) world
                          current = world !! y !! x

expand :: [[Int]] -> [[Int]]
expand [] = []
expand world = transpose $ (transpose . transpose) [sideBar] ++ transpose ([topBar] ++ world ++ [topBar]) ++ [sideBar]
  where topBar = [0 | _ <- [0 .. length (head world) - 1]]
        sideBar = [0 | _ <- [0 .. length world + 1]]

compact :: [[Int]] -> [[Int]]
compact [] = [[]]
compact l = (transpose . transpose . compact' .compact') l
  where compact' = transpose . reverse . removeEmptyLine . reverse . removeEmptyLine
        removeEmptyLine = dropWhile (\w -> 0 == sum w)

neighbors :: Pos -> [[Int]] -> [Pos]
neighbors (x, y) world = filter onMap [(dx + x, dy + y) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]
  where onMap (x,y) = x `elem` [0 .. length (head world) - 1] && y `elem` [0 .. length world - 1]


construct :: Int -> [(Pos, Int)] -> [[Int]]
construct rowLength world = compact $ splitEvery rowLength [ life | (_, life) <- sort world]

splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list

htmlize :: [[Int]] -> String
htmlize = concatMap ((++ "<br>") . concatMap cell)
  where cell n = if n > 0 then "&#x2593;&#x2593;" else "&#x2591;&#x2591;"
