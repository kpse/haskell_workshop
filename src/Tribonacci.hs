module Tribonacci
    where

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n | n <= 3 = take n [a, b, c]
tribonacci (a, b, c) n = let arr = tribonacci (a, b, c) (n - 1)
  in arr ++ [generate arr]


generate :: Num a => [a] -> a
generate = sum . take 3 . reverse

-- tribonacci _ n | n < 1 = []
-- tribonacci (a, b, c) n = a : tribonacci (b, c, a+b+c) (n-1)
