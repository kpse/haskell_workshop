module Spinning where

import           Data.String (words)

spinWords :: String -> String
spinWords = unwords . map singleWord . words 

singleWord :: String -> String
singleWord word | length word < 5 = word
singleWord word = reverse word
