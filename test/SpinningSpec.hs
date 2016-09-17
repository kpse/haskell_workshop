module SpinningSpec where
import Spinning (spinWords)
import Test.Hspec

spec = describe "spinWords" $ do
    it "should work for some examples" $ do
      spinWords "Hey fellow warriors" `shouldBe` "Hey wollef sroirraw"
      spinWords "Test"                `shouldBe` "Test"
      spinWords "Example"             `shouldBe` "elpmaxE"
      spinWords "Test Example"        `shouldBe` "Test elpmaxE"
