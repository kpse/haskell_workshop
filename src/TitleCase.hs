module TitleCase
    where

import Data.List (map)
import Data.Char (toUpper, toLower)

titleCase :: String -> String -> String
titleCase minor [] = []
titleCase minor title = unwords $ captialWord (head (words title)) : foldl (transform minor) [] (tail $ words title)

captialWord :: String -> String
captialWord [] = []
captialWord (x:xs) = toUpper x : map toLower xs

lowerString :: String -> String
lowerString (x:xs) = toLower x : map toLower xs

checkWords :: String -> String -> Bool
checkWords minor word = lowerString word `elem` map lowerString (words minor)

transform :: String -> [String] -> String -> [String]
transform minor acc word = case checkWords minor word of
  True -> acc ++ [lowerString word]
  False -> acc ++ [captialWord word]
