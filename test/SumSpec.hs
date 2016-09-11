module SumSpec where

import Lib

import Test.Hspec

spec = describe "Deal with 2 numbers" $
    it "should sum 2" $ sum' 1 2 `shouldBe` 3
