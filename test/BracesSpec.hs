module BracesSpec where
import Braces (validBraces)
import Test.Hspec

spec = describe "validBraces" $ do
    it "should work for some examples" $ do
      validBraces "()"             `shouldBe` True
      validBraces "[([)"           `shouldBe` False
      validBraces "())({}}{()][][" `shouldBe` False
      validBraces "({})[({})]"     `shouldBe` True
    it "should work with nested" $
      validBraces "(([]()(){}){(){{}}{{}}})" `shouldBe` True
