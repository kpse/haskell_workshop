module UnlimitedGameOfLife where

type POS = (Int, Int)
data LIVE = LIVE | DEAD

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration input gen = [[0,1,0],
         [0,0,1],
         [1,1,1]]

round :: POS -> [[Int]] -> Int
round (x,y) world = 0

htmlize :: [[Int]] -> String
htmlize = concatMap ((++ "<br>") . concatMap cell)
  where cell n = if n > 0 then "&#x2593;&#x2593;" else "&#x2591;&#x2591;"
