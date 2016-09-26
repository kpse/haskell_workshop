module Braces where

import Data.List (find)

validBraces :: String -> Bool
validBraces [] = True
validBraces input = case head input of
  open | open `elem` allOpenBrackets ->
    case closeAndAfter of
      (closeBracket:rest) | closeBracketFor open == Just closeBracket ->
        validBraces inBrackets && validBraces rest
      _ -> False
    where (inBrackets,closeAndAfter) = span ((/= closeBracketFor open) . Just) $ tail input
  _ -> False

closeBracketFor :: Char -> Maybe Char
closeBracketFor c = find ((c ==) . head) brackets >>= Just . last

brackets = ["()", "[]", "{}"]
allOpenBrackets = map head brackets
