module MultiplesOf3And5 where

import Data.List (nub)

solution :: Integer -> Integer
solution number = sum $ nub $ takeWhile (<number) [3, 6 ..] ++ takeWhile (<number) [5, 10 ..]
