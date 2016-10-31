module PointInPolygonSpec where
import PointInPolygon (pointInPoly)
import Data.List (intercalate)
import Test.Hspec

spec = describe "main" $ do
  it "Should handle a simple square" $ do
    let poly = [ (-5, -5), (5, -5)
               , (5, 5), (-5, 5)
               ]
    showAndTest poly (-6,0) False
    showAndTest poly (1,1) True

showAndTest poly point expect = do
  drawTest poly point expect
  pointInPoly poly point `shouldBe` expect

drawTest poly point inside = putStrLn div
  where
    div = concat [ "<div style='background:white; width:140px;'>"
                 , "<svg width='140' height='140'>"
                 , "<polygon points='" ++ intercalate " " points ++ "' stroke='blue' fill='white'></polygon>"
                 , "<circle cx='" ++ show cx ++ "' cy='" ++ show cy ++ "' r='2' fill='" ++ color ++ "'></circle>"
                 , "</svg>"
                 , "</div>"
                 ]
    points = map (showPt . transform) poly
    showPt (x,y) = show x ++ "," ++ show y
    (cx,cy) = transform point
    transform (x,y) = (t x, t y) where t i = (i + 7) * 10 + 0.5
    color = if inside then "green" else "red"
