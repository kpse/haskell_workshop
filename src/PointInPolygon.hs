module PointInPolygon where

import Data.List (sort)

type Point = (Double, Double)

pointInPoly :: [Point] -> Point -> Bool
pointInPoly poly point = inBound (fst point) xs && inBound (snd point) ys
  where
    xs = allX(poly)
    ys = allY(poly)

allX :: [Point] -> (Double, Double)
allX poly = (head allC, last allC)
  where
    allC = sort $ map fst poly

allY :: [Point] -> (Double, Double)
allY poly = (head allC, last allC)
  where
    allC = sort $ map snd poly

inBound :: Double -> (Double, Double) -> Bool
inBound p (l, r) = l < p && p < r
