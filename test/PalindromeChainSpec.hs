module PalindromeChainSpec where
import PalindromeChain (palindromeChainLength)
import Test.Hspec

spec = describe "palindromeChainLength" $ do
    it "should work for 87 -> 4" $ do
      palindromeChainLength 87 `shouldBe` 4
