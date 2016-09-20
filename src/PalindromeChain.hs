module PalindromeChain (
  palindromeChainLength,
  palindromeChain,
  isPalindrome
) where

palindromeChainLength :: Integer -> Integer
palindromeChainLength = toInteger . length . palindromeChain

palindromeChain :: Integer -> [Integer]
palindromeChain number = case isPalindrome number of
  True -> []
  False -> number : palindromeChain (number + (read .reverse . show) number)

isPalindrome :: Integer -> Bool
isPalindrome number = let str = show number in case str of
  [] -> True
  [x] -> True
  [x, y] -> x == y
  _ -> head str == last str && isPalindrome (read ((tail . init) str) :: Integer)
