module MaximumSpec where

import Lib

import Test.Hspec

spec = describe "calculate the maximum number" $ do
  describe "first describe" $ do
    it "should give nothing if empty input" $
      maximum' ([]::[Int]) `shouldBe` Nothing
    it "should give out the maximum" $
      maximum' [1] `shouldBe` Just 1
    it "should give out the maximum" $ do
      maximum' [1, 2] `shouldBe` Just 2
      maximum' [3, 1, 2] `shouldBe` Just 3
  describe "second describe" $
    it "should give nothing if empty input" $
      maximum' [] `shouldBe` (Nothing :: Maybe Int)
