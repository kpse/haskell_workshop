module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib (-1) = 1
fib (-2) = -1

fib n | n < 0 = fib' (-1) 1 2 (n + 3)
    where fib' pre pre2 result count = case count of
                                        x | x < 0 -> fib' result pre (pre - result) (x + 1)
                                        _ -> result

fib n = fib' 1 0 0 (n - 1)

fib' pre pre2 result count = case count of
  x | x > 0 -> fib' (pre + pre2) pre (pre + pre2) (x - 1)
  _ -> result
