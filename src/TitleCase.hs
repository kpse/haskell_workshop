module TitleCase
    where

import Data.Char (toUpper, toLower)

titleCase :: String -> String -> String
titleCase minor [] = []
titleCase minor title = unwords $ captialWord (head (words title)) : foldl (transform minor) [] (tail $ words title)

captialWord :: String -> String
captialWord [] = []
captialWord (x:xs) = toUpper x : map toLower xs

lowerString :: String -> String
lowerString arr@(x:xs) = map toLower arr

checkWords :: String -> String -> Bool
checkWords minor word = lowerString word `elem` map lowerString (words minor)

transform :: String -> [String] -> String -> [String]
transform minor acc word = if checkWords minor word
  then acc ++ [lowerString word]
  else acc ++ [captialWord word]
