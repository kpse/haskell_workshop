module TribonacciSpec where

import Tribonacci (tribonacci)
import Test.Hspec
import Test.QuickCheck

spec = describe "Tribonacci" $
  it "should work for some examples" $ do
    tribonacci (1, 1, 1) 10 `shouldBe` [1,1,1,3,5,9,17,31,57,105]
    tribonacci (0, 0, 1) 10 `shouldBe` [0,0,1,1,2,4,7,13,24,44]
    tribonacci (0, 1, 1) 10 `shouldBe` [0,1,1,2,4,7,13,24,44,81]