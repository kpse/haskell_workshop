module SnailSpec where
import Snail (snail)
import Test.Hspec

spec = describe "Snail" $ do
  it "First example" $ do
    let array = [[1,2,3],
                 [4,5,6],
                 [7,8,9]]
        expected = [1,2,3,6,9,8,7,4,5]
    snail array `shouldBe` expected

  it "Second example" $ do
    let array = [[1,2,3],
                 [8,9,4],
                 [7,6,5]]
        expected = [1,2,3,4,5,6,7,8,9]
    snail array `shouldBe` expected

  it "1x1 example" $ do
    let array = [[1]]
        expected = [1]
    snail array `shouldBe` expected
  it "5x5 example" $ do
    let array = [[1,2,3,4,5],
                [6,7,8,9,10],
                [11,12,13,14,15],
                [16,17,18,19,20],
                [21,22,23,24,25]]
        expected = [1,2,3,4,5,10,15,20,25,24,23,22,21,16,11,6,7,8,9,14,19,18,17,12,13]
    snail array `shouldBe` expected
