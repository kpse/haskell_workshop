module Braces where

import Data.List (find)

validBraces :: String -> Bool
validBraces [] = True
validBraces input = case head input of
  open | open `elem` allOpenBrackets ->
    case closeAndAfter of
      (closeBracket:rest) | closeBrackets open == Just closeBracket ->
        validBraces inBrackets && validBraces rest
      _ -> False
    where (inBrackets,closeAndAfter) = span ((/= closeBrackets open) . Just) $ tail input
  _ -> False

closeBrackets :: Char -> Maybe Char
closeBrackets c = find ((c ==) . head) brackets >>= Just . last

brackets = ["()", "[]", "{}"]
allOpenBrackets = map head brackets
