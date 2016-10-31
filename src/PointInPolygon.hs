module PointInPolygon where

type Point = (Double, Double)

pointInPoly :: [Point] -> Point -> Bool
pointInPoly poly point = all (>= 0) tolerate || all (<= 0) tolerate
  where
    tolerate = map (\w -> if (abs(w) < 0.0000001) then 0 else w) res
    res = allGradients poly point

gradient :: Point -> Point -> Point -> Double
gradient (x,y) (x1, y1) (x2, y2) = (x2-x1)*(y-y1) - (y2-y1)*(x-x1)

allGradients :: [Point] -> Point -> [Double]
allGradients points p = map (\(p1, p2) -> gradient p p1 p2) pairs
  where
    pairs = zip points $ (tail points) ++ [head points]
