module SplitString
 where

solution :: String -> [String]
solution [] = []
solution str = case str of
  x:x1:xs -> take 2 str : solution xs
  [x] -> [x : "_"]
  [] -> []

import Data.List.Split

solution2 = takeWhile ((2 ==) . length) . chunksOf 2 . (++ "_")
