module Duplicates (
  duplicateCount
) where

import           Data.Char (toLower)
import           Data.Map  (Map)
import qualified Data.Map  as Map

duplicateCount :: String -> Int
duplicateCount [] = 0
duplicateCount string = Map.size $ filterOut $ foldl createMap Map.empty string

createMap :: Map Char Int -> Char -> Map Char Int
createMap m c = let lowerC = toLower c in
  case Map.lookup lowerC m of
    Just x -> Map.insert lowerC (x+1) m
    Nothing -> Map.insert lowerC 1 m

filterOut :: Map Char Int -> Map Char Int
filterOut = Map.filter (>1)
