module Permutations
 where

import Data.List (nub)

permutations :: String -> [String]
permutations "" = [""]
permutations str = nub [ make str order | order <- generate (length str) []]

make :: String -> [Int] -> String
make str = map (str !!)

generate :: Int -> [Int] -> [[Int]]
generate n res = case [ res ++ [x] | x <- [0..n-1], x `notElem` res] of
    current@(x:xs) | length x == n -> current
    current -> current >>= generate n
