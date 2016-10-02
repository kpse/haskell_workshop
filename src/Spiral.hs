module Spiral where

import Data.List (transpose)

spiralize :: Int -> [[Int]]
spiralize = walkOn 1 1 . square

square :: Int -> [[Int]]
square 0 = []
square size = [[0 | x <- [0 .. size - 1]] | y <- [0 .. size - 1]]

walkOn :: Int -> Int -> [[Int]] -> [[Int]]
walkOn _ _ [] = []
walkOn turn c [x] = if turn `mod` 4 > 2 then [[0]] else [[c]] 
walkOn turn c (x:xs) = walk : (transpose . reverse . walkOn (turn + 1) nextToken) rest
  where
    walk = if turn `mod` 4 == 0 then init (map (const c) x) ++ [switch c] else map (const c) x
    nextToken = if turn `mod` 4 == 0 then switch c else c
    rest = (reverse .transpose) xs

switch :: Int -> Int
switch 1 = 0
switch 0 = 1
