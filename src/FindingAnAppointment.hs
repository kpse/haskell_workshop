module FindingAnAppointment where

import Data.List (find)
import Text.Printf

getStartTime :: [[(String, String)]] -> Int -> Maybe String
getStartTime schedules duration = found >>= \w -> return $ (display . head) w
  where
    found = find ((>=duration) .length) timeLine
    timeLine = foldl available [[9*60..19*60]] schedules

available :: [[Int]] -> [(String, String)] ->  [[Int]]
available [] _ = []
available a [] = a
available timeLine (x:xs) = available (cutOut timeLine x) xs

cutOut :: [[Int]] -> (String , String) -> [[Int]]
cutOut [] _  = []
cutOut input (start,end) = filter ((/=)0 . length) $ concatMap slice input
  where slice piece = takeWhile (< toMinutePoint start) piece : [dropWhile (< toMinutePoint end) piece]

toMinutePoint :: String -> Int
toMinutePoint m = (read (take 2 m) :: Int) * 60 + read (drop 3 m) :: Int

display :: Int -> String
display x = printf "%02d:%02d" (x `div` 60) (x `mod` 60)
