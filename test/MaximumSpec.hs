module MaximumSpec where

import Lib

import Test.Hspec

spec = describe "calculate the maximum number" $ do
    it "should give nothing if empty input" $ do
      maximum' ([] :: [Int]) `shouldBe` (Nothing :: Maybe Int)
      maximum' [1] `shouldBe` Just 1
      maximum' [1, 2] `shouldBe` Just 2
      maximum' [3, 1, 2] `shouldBe` Just 3
