module Lib
    (
    	someFunc,
      sum',
      maximum'
    )
    where

someFunc :: IO ()
someFunc = putStrLn "This function is the best!"


sum' :: (Num a) => a -> a -> a
sum' a b = a + b

maximum' :: (Ord a) => [a] -> Maybe a
maximum' [] = Nothing
