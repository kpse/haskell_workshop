module Permutations
 where

import Data.List (nub)

permutations :: String -> [String]
permutations "" = [""]
permutations str = nub [ make str order | order <- gen2 (length str) []]

make :: String -> [Int] -> String
make str = map (str !!)

generate :: Int -> [Int] -> [[Int]]
generate n res = [ res ++ [x] | x <- [0..n-1], x `notElem` res]
gen2 :: Int -> [[Int]] -> [[Int]]
gen2 n [] = gen2 n $ generate n []
gen2 n l = case l of
  (x:xs) | length x == n -> l
  _ -> gen2 n $ concatMap (generate n) l
