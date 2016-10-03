module LastDigit where

import Data.List (isInfixOf)

lastDigit :: [Integer] -> Integer
lastDigit [] = 1
lastDigit [x] = x `mod` 10
lastDigit [x,0] = 1
lastDigit (1:xs) = lastDigit []
lastDigit (x:arr) | 0 `elem` arr = case p of
    0 -> lastDigit [x,0]
    a -> lastDigit [x,a]
  where
    p = foldr (^) 1 $ remove00 arr
lastDigit arr = lastDigit' (arr ++ [1])

lastDigit' :: [Integer] -> Integer
lastDigit' (x:p:xs) | p `mod` 4 == 1 = x `mod` 10
lastDigit' (x:p:n:xs) | p `mod` 4 == 2 && n == 1 = x ^ 2 `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 2 = x ^ 4 `mod` 10
lastDigit' (x:p:e:xs) | p `mod` 4 == 3 && e `mod` 2 == 0 = x `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 3 = x ^ 3 `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 0 = x ^ 4 `mod` 10

remove00 :: [Integer] -> [Integer]
remove00 arr | [0,0] `isInfixOf` arr = (reverse .replace0. reverse) arr
remove00 arr = replace0 arr

replace0 :: [Integer] -> [Integer]
replace0 [] = []
replace0 (0:0:xs) = 1 : replace0 xs
replace0 (x:xs) = x : replace0 xs
