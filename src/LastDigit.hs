module LastDigit where

import Data.List (isInfixOf)

lastDigit :: [Integer] -> Integer
lastDigit [] = 1
lastDigit [x] = x `mod` 10
lastDigit [x, y] = x ^ y `mod` 10
lastDigit arr@(0:xs) = foldr (^) 1 arr `mod` 10
lastDigit arr = lastDigit' $ arr ++ replicate 3 1

lastDigit' :: [Integer] -> Integer
lastDigit' arr | 0 `elem` arr = foldr (^) 1 arr `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 1 = x `mod` 10
lastDigit' (x:p:n:xs) | p `mod` 4 == 2 && n == 1 = x ^ 2 `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 2 = x ^ 4 `mod` 10
lastDigit' (x:p:e:xs) | p `mod` 4 == 3 && e `mod` 2 == 0 = x `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 3 = x ^ 3 `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 0 = x ^ 4 `mod` 10
