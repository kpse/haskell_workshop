module SumDigPow where

import           Data.Char (digitToInt)

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = filter (\n -> n == foldl calc 0 (zip [1..](show n))) [a .. b]

calc :: Int -> (Int, Char) -> Int
calc acc (pos, num) = acc + digitToInt num ^ pos
