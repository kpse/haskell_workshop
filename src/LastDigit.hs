module LastDigit where

lastDigit :: [Integer] -> Integer
lastDigit [] = 1
lastDigit [x] = x `mod` 10
lastDigit [x,1] = lastDigit [x]
lastDigit (1:xs) = lastDigit []
lastDigit [x,0] = lastDigit []
lastDigit [0,0,0] = 0
lastDigit arr = lastDigit' (arr ++ [1])

lastDigit' :: [Integer] -> Integer
lastDigit' (x:p:xs) | p `mod` 4 == 1 = x `mod` 10
lastDigit' (x:p:n:xs) | p `mod` 4 == 2 && n == 1 = x ^ 2 `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 2 = x ^ 4 `mod` 10
lastDigit' (x:p:e:xs) | p `mod` 4 == 3 && e `mod` 2 == 0 = x `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 3 = x ^ 3 `mod` 10
lastDigit' (x:p:xs) | p `mod` 4 == 0 = x ^ 4 `mod` 10
