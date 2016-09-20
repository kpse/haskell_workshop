module Merge where

merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = quicksort $ xs ++ ys

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs
