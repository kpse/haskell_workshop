module PointInPolygon where

import Data.List (intercalate)

type Point = (Double, Double)
type Line = (Point, Point)

pointInPoly :: [Point] -> Point -> Bool
pointInPoly poly point = odd intersactions
  where
    intersactions = length $ filter rayTrace lines
    lines = zip poly (tail $ cycle poly)
    rayTrace = isIntersaction ((-100, snd point), point)

isIntersaction :: Line -> Line -> Bool
isIntersaction (a, b) (c, d) =  sig r1 r2 && sig r3 r4
  where
    sig l r = l * r < 0
    r1 = abCrossAc a b c
    r2 = abCrossAc a b d
    r3 = abCrossAc c d a
    r4 = abCrossAc c d b

abCrossAc :: Point -> Point -> Point -> Double
abCrossAc (x1, y1) (x2, y2) (x3, y3) = cross ((x2 - x1), (y2 - y1)) ((x3 - x1), (y3 - y1))
  where
    cross (x1,y1) (x2,y2) = x1 * y2 - x2 * y1
