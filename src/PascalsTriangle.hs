module PascalsTriangle where

pascalsTriangle :: Int -> [Int]
pascalsTriangle n = concat [ pascalsTriangle' x | x <- [1..n]]

pascalsTriangle' :: Int -> [Int]
pascalsTriangle' 1 = [1]
pascalsTriangle' n = [1] ++ zipWith (+) lastLine (tail lastLine) ++ [1]
  where
    lastLine = pascalsTriangle' (n-1)
