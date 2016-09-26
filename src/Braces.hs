module Braces where

import Data.List

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
  where result = replace p input

replace :: String -> String -> String
replace _ [] = []
replace p (x1:x2:xs) | p == [x1,x2] = replace p xs
replace p (x:xs) = x : replace p xs
