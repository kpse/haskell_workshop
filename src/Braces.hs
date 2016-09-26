module Braces where

validBraces :: String -> Bool
validBraces [] = True
validBraces input = case head input of
  open | open `elem` brackets ->
    case end of
      end@(x:xs) | closeBrackets open == Just x ->
        validBraces inBracket && validBraces xs
      _ -> False
    where (inBracket,end) = span (\w -> Just w /= closeBrackets open) $ tail input
  _ -> False

closeBrackets :: Char -> Maybe Char
closeBrackets '(' = Just ')'
closeBrackets '{' = Just '}'
closeBrackets '[' = Just ']'
closeBrackets _ = Nothing

brackets = "()[]{}"
