module MaximumSpec where

import Lib

import Test.Hspec

spec = describe "calculate the maximum number" $ do
    it "should give nothing if empty input" $
      maximum' ([] :: [Int]) `shouldBe` (Nothing :: Maybe Int)
