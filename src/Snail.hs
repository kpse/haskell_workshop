module Snail
  (snail)
  where

import Data.List (transpose)

snail :: [[Int]] -> [Int]
snail [] = []
snail (x:xs) = x ++ (snail . reverse . transpose) xs
