module Main where

import Lib

main :: IO ()
main = do
   print (newString ++ show(fun 2) ++ show(head' [1,2,3]) ++ show(head' [12,3,2,31]))
   let x = ([] :: [Int])
   print $ show(head' x)
   print (myMap (\t -> 2 * t) [12,3,3,4,4,5,6,7])
   print (myFilter (\t -> t > 10) [12,3,3,4,4,5,6,27])
   print (myFold (+) 90000 [123, 312])
   print (myMultiply 1 4 6 8)
   print (listB)
   print $ addList [1,2,3,4,5]
   print $ show(head2' x)
   print $ show(head2' [1])
   print $ myInt
   print $ myList
   print $ incrementAndShow 1
   print $ rating Intro
   print $ rating Intermediate
   print $ rating Expert
   print $ review Expert
   --print $ rating ""
   --print $ rating "Nothing"
   --print $ rating "Good"
   putStrLn "estimation!" >> putStrLn "WIP!" >> putStrLn "Done!"
   -- getLine >>= (\name -> putStrLn ("Hello " ++ name))
   -- myLiftIO (\(a, b) -> a + b)



-- variables
add = 1

newString = "my number is "

tuple = (1, "success")

-- new list

list1 = [1,2,3]
list2 = 4:list2

-- functions
fun x = x + 2


listf :: [a] -> Int
listf [] = 0
listf (x:xs) = 1 + listf xs

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x



myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter f [] = []
myFilter f (x:xs) = if f x then x : myFilter f xs else myFilter f xs

myFold :: (a -> b -> b) -> b -> [a] -> b
myFold f res [] = res
myFold f res (x:xs) = f x (myFold f res xs)


myMultiply :: Int -> Int -> Int -> Int -> Int
myMultiply a b c d = a * b * c * d


listA :: ([]) Int
listA = [1]

listB :: [(Int, Int)]
--listB = [("1",2), (3,4)]
listB = [(1,2), (3,4)]


-- ADT
data MyResult a = Something a | NotSure
instance (Show a) => Show(MyResult a) where
          show (Something x)       = "this is definitely a " ++ show(x)
          show (NotSure)   =  "not sure about this one"

head2' :: [a] -> MyResult a
head2' [] = NotSure
head2' (x:xs) = Something x

addList :: Num a => [a] -> [a]
addList [] = []
addList (x:xs) = x + 1 : addList(xs)


-- read
myInt :: Int
myInt = read "1"

myList :: [Int]
myList = read "[1,2,3,4]"


incrementAndShow :: (Num a, Show a) => a -> String
incrementAndShow a = show(a + 1)

data Rating =
  SoAwesomeICried |
  PrettyCool      |
  Meh             |
  ForTheLoveOfGodMakeItEnd
  deriving Show

class Rateable r where
  rating :: r -> Rating


data Beer = Coopers | Fosters | CarltonDraught
  deriving Show

instance Rateable Beer where
  rating Coopers = PrettyCool
  rating Fosters = ForTheLoveOfGodMakeItEnd
  rating CarltonDraught = Meh


data Movie = Movie String
  deriving Show

instance Rateable Movie where
  rating (Movie "Bladerunner") = SoAwesomeICried
  rating (Movie "Tron") = PrettyCool
  rating _ = Meh


data HaskellWorkshop = Intro | Intermediate | Expert
  deriving Show

instance Rateable HaskellWorkshop where
  rating Intro = Meh
  rating Intermediate = SoAwesomeICried
  rating _ = ForTheLoveOfGodMakeItEnd

class Reviewable r where
  review :: r -> String

instance Reviewable HaskellWorkshop where
  review _ = "review comment: Impressive!"



-- instance Rateable [Char] where
--   rating _ = Meh


-- myLiftIO :: (a -> b) -> IO a -> IO b
-- myLiftIO f =
