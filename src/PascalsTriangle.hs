module PascalsTriangle where

pascalsTriangle :: Int -> [Int]
pascalsTriangle 1 = [1]
pascalsTriangle 2 = [1, 1]
pascalsTriangle 3 = [1, 2, 1]
pascalsTriangle n = prev ++ [1, 3, 3, 1]
  where prev = concat [ pascalsTriangle x | x <- [1..n - 1]]
