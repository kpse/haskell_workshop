module Lib
    (
    	someFunc,
      sum'
    )
    where

someFunc :: IO ()
someFunc = putStrLn "This function is the best!"


sum' :: (Num a) => a -> a -> a
sum' a b = a + b
