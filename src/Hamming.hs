module Hamming where

import Data.List (sort, nub)

hamming  :: Int -> Int
hamming n = head $ drop n $ hammingChain 17

hammingChain :: Int -> [Int]
hammingChain 0 = [0]
hammingChain n = sort $ nub $ filter (>0) [eval x y z | x <- [0..n], y <- [0..n], z <- [0..n]] ++ hammingChain (n - 1)

eval :: Int -> Int -> Int -> Int
eval a b c = 2 ^ a * 3 ^ b * 5 ^ c
