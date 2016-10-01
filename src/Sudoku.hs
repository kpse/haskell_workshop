module Sudoku where

import Data.List (transpose, find, nub)
import Data.Maybe

type Line = [Int]
type Table = [Line]

sudoku :: Table -> Table
sudoku source = fromMaybe [] res
  where
    res = find correct $ randomFill source

correct :: Table -> Bool
correct source = allEqual countPerLine && allEqual conunPerRow
  where
    allEqual line = maximum line == minimum line
    countPerLine = map sum source
    conunPerRow = map sum $ transpose source

randomFill :: Table -> [Table]
randomFill table = case other of
  [] -> [table]
  (thieLine:rest) -> concatMap randomFill [start ++ [x] ++ rest | x <- lineReplace table row thieLine]
  where
    row = length start
    (start, other) = span (notElem 0) table

subTableOf :: Table -> Int -> Int -> Line
subTableOf table x y = concatMap (take 3 . drop (3 * xIndex)) $ (take 3 . drop (3 * yIndex)) table
  where
    xIndex = x `div` 3
    yIndex = y `div` 3

lineReplace :: Table -> Int -> Line -> [Line]
lineReplace table row line = case other of
  [] -> [line]
  (zero:rest) -> concatMap (lineReplace table row) [ start ++ [x] ++ rest | x <- pick]
  where
    pick = [ x | x <- [1..9], x `notElem` line, x `notElem` chosen, x `notElem` col]
    chosen = subTableOf table (length start) row
    col = transpose table !! length start
    (start, other) = span (/=0) line
