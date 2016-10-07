module Braces where

import           Data.List

validBraces :: String -> Bool
validBraces [] = True
validBraces [x] = False
validBraces input | length (filter (`elem` openBrackets) input) /= length (filter (`elem` closeBrackets) input) = False
validBraces input = all (==True) $ map check brackets
  where check p = ((== []) . remove p. filter (`elem` p)) input


brackets = ["()", "[]", "{}"]
openBrackets = map head brackets
closeBrackets = map last brackets

remove :: String -> String -> String
remove _ [] = []
remove p input = if length result < length input then remove p result else result
  where result = remove2Chars p input

remove2Chars :: String -> String -> String
remove2Chars _ [] = []
remove2Chars p (x1:x2:xs) | p == [x1,x2] = remove2Chars p xs
remove2Chars p (x:xs) = x : remove2Chars p xs
