module LastDigit where

import Data.List (isInfixOf)

lastDigit :: [Integer] -> Integer
lastDigit [] = 1
lastDigit [x] = x `mod` 10
lastDigit [x, y] = x ^ y `mod` 10
lastDigit arr@(x:y:xs) | all (==0) xs && (odd.length) xs = x `mod` 10
lastDigit arr@(x:y:xs) | all (==0) xs = x ^ y `mod` 10
lastDigit arr@(x:y:xs) | (odd.length) (takeWhile (==0) xs) = x `mod` 10
lastDigit arr@(x:0:xs) | null (takeWhile (==0) xs) = 1
lastDigit arr@(x:0:xs) | (not.null) (takeWhile (==0) xs) = x `mod` 10
lastDigit arr = lastDigit' $ simplify arr ++ replicate 3 1

lastDigit' :: [Integer] -> Integer
lastDigit' (x:p:xs) | p `mod` 4 == 1 = x `mod` 10
lastDigit' (x:p:n:xs) | p `mod` 4 == 2 && n == 1 = x ^ 2 `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 2 = x ^ 4 `mod` 10
lastDigit' (x:p:e:xs) | p `mod` 4 == 3 && e `mod` 2 == 0 = x `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 3 = x ^ 3 `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 0 = x ^ 4 `mod` 10

simplify :: [Integer] -> [Integer]
simplify arr = initArr ++ [foldr (^) 1 zeroEnd]
  where
    zeroEnd = takeWhile (`elem` [0, 1]) $ reverse arr
    initArr = reverse $ dropWhile (`elem` [0, 1]) $ reverse arr
