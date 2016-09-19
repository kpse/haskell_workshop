module Snail where

import Data.List (nub)

data Direction = R | D | L | U

snail :: [[Int]] -> [Int]
snail [[]] = []
snail [[x]] = [x]
snail (x:xs) | length (x:xs) /= length x = []
snail (x:xs) = let w = length x
                  in
                  map (\p -> uncurry move p (x:xs)) $ nextMove [(0, 0)] 0 w 0 w R

move :: Int -> Int -> [[Int]] -> Int
move x y = head . drop x . head . drop y

nextMove :: [(Int, Int)] -> Int -> Int -> Int -> Int -> Direction -> [(Int, Int)]
nextMove path minW width minH height d = case (last path, d) of
  _ | width == minW || height == minH -> path
  ((x, y), R) | x < width - 1 -> nextMove (path ++ [(x + 1, y)]) minW width minH height R
  ((x, y), R) -> nextMove path minW width (minH+1) height D -- turn
  ((x, y), D) | y < height - 1 -> nextMove (path ++ [(x, y + 1)]) minW width minH height D
  ((x, y), D) -> nextMove path minW (width-1) minH height L -- turn
  ((x, y), L) | x > minW -> nextMove (path ++ [(x - 1, y)]) minW width minH height L
  ((x, y), L) -> nextMove path minW width minH (height-1) U -- turn
  ((x, y), U) | y > minH -> nextMove (path ++ [(x, y - 1)]) minW width minH height U
  ((x, y), U) -> nextMove path (minW+1) width minH height R -- turn

-- snail (xs:xss) = xs ++ (snail . reverse . transpose) xss
