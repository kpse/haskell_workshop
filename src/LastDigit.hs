module LastDigit where

lastDigit :: [Integer] -> Integer
lastDigit [] = 1
lastDigit [x] = x `mod` 10
lastDigit [x,y] | x + y < 20 = x ^ y `mod` 10
lastDigit arr |  1 `elem` arr = lastDigit $ simplify arr
lastDigit arr |  0 `elem` arr = lastDigit $ simplify arr
lastDigit arr = algorithm $ simplify arr ++ replicate 3 1

algorithm :: [Integer] -> Integer
algorithm (x:p:xs)   | p `mod` 4 == 1 = x `mod` 10
algorithm (x:p:n:xs) | p `mod` 4 == 2 && n == 1 = x ^ 2 `mod` 10
algorithm (x:p:xs)   | p `mod` 4 == 2 = x ^ 4 `mod` 10
algorithm (x:p:e:xs) | p `mod` 4 == 3 && e `mod` 2 == 0 = x `mod` 10
algorithm (x:p:xs)   | p `mod` 4 == 3 = x ^ 3 `mod` 10
algorithm (x:p:xs)   | p `mod` 4 == 0 = x ^ 4 `mod` 10

simplify :: [Integer] -> [Integer]
simplify [a] = [a]
simplify arr | (foldable.last) arr || (foldable.last) start = simplify $ init start ++ [ last start ^ last arr]
  where
    start = init arr
    foldable a = a `elem` [0, 1]

simplify arr | 1 `elem` arr = takeWhile (/=1) arr
simplify arr | 0 `elem` arr = simplify $ reverse $ dropWhile (/=0) $ reverse arr

simplify arr = arr
