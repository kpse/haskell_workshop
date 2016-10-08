module PascalsTriangleSpec where
import PascalsTriangle (pascalsTriangle)
import Test.Hspec
import Test.QuickCheck

spec = describe "pascalsTriangle" $ do
    it "should work for some examples" $ do
      pascalsTriangle 1  `shouldBe` [1]
      pascalsTriangle 4 `shouldBe` [1,1,1,1,2,1,1,3,3,1]
