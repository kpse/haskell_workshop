module SumSpec where

import Lib

import Test.Hspec

spec = describe "Deal with 2 numbers" $ do
    it "should sum 1 and 2" $
      sum' 1 2 `shouldBe` 3

    it "should sum 3 and 4" $
      sum' 3 4 `shouldBe` 7
