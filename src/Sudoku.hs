module Sudoku where

import Data.List (transpose, find)
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
  (zero:rest) -> concatMap randomFill [start ++ [x] ++ rest | x <- lineReplace table zero]
  where
    row = transpose table !! length start
    (start, other) = span (notElem 0) table


lineReplace :: Table -> Line -> [Line]
lineReplace table line = case other of
  [] -> [line]
  (zero:rest) -> concatMap (lineReplace table) [ start ++ [x] ++ rest | x <- pick]
  where
    pick = [ x | x <- [1..9], x `notElem` line, x `notElem` row]
    row = transpose table !! length start
    (start, other) = span (/=0) line
