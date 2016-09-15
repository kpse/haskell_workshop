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
maximum' (x:xs) = case maximum' xs of
    Nothing -> Just x
    Just v -> if x > v then Just x else Just v

    
