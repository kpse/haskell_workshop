module Spinning where

import           Data.String (words)

spinWords :: String -> String
spinWords str = unwords $ map singleWord (words str)

singleWord :: String -> String
singleWord word = case length word of
  x | 5 <= x -> reverse word
  _ -> word
