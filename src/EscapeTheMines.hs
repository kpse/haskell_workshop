module EscapeTheMines where

import Data.List (minimumBy)
import Data.Function (on)

type XY = (Int,Int)
data Move = U | D | R | L deriving (Eq, Show)

solve :: [[Bool]] -> XY -> XY -> [Move]
solve grid miner exit = if miner == exit then []
  else minimumBy (compare `on`length) $ filter (\x -> follow x miner == exit) $ search grid miner (==exit) []

search :: [[Bool]] -> XY -> (XY -> Bool) -> [Move] -> [[Move]]
search grid pos end path = if (end pos) || (length path > 10) then [path] else
  let pathes = [ shouldGo path m | m <- exits grid pos] in case pathes of
    [] -> [path]
    x:xs -> concatMap (\p -> search grid (go pos (last p)) end p) $ filter (not.null) pathes


exits :: [[Bool]] -> XY -> [Move]
exits grid miner = filter (canGo grid miner) [U, R, D, L]

shouldGo :: [Move] -> Move -> [Move]
shouldGo [] m = [m]
shouldGo moves m = if isReverse (last moves) m then [] else moves ++ [m]

isReverse :: Move -> Move -> Bool
isReverse D U = True
isReverse U D = True
isReverse R L = True
isReverse L R = True
isReverse _ _ = False

inRange :: [[Bool]] -> XY -> Move -> Bool
inRange grid (row, col) move = case move of
    L -> row > 0
    R  -> row < length grid - 1
    U -> col > 0
    D | row > length grid - 1 -> False
    D -> col < (length . head . drop row) grid - 1

canGo :: [[Bool]] -> XY -> Move -> Bool
canGo grid (row, col) move = inRange grid (row, col) move && accessible (go (row, col) move) grid

go :: XY -> Move -> XY
go (row, col) move = case move of
  L -> (row - 1, col)
  R  -> (row + 1, col)
  U -> (row, col - 1)
  D -> (row, col + 1)

follow :: [Move] -> XY -> XY
follow [] a = a
follow moves start = last $ foldl (\line m -> line ++ [go (last line) m]) [start] moves

accessible :: XY -> [[Bool]] -> Bool
accessible _ [] = False
accessible (row, _) grid | row > length grid - 1 = False
accessible (row, col) grid | col > length ((head . drop row) grid) - 1 = False
accessible (row, col) grid = (head . drop col . head . drop row) grid
