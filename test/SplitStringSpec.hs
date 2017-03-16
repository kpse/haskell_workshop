module SplitStringSpec where
import SplitString (solution)
import Test.Hspec

spec = describe "solution" $ do
    it "gives the correct result for \"abc\"" $
      solution "abc" `shouldBe` ["ab", "c_"]
    it "gives the correct result for \"abcdef\"" $
      solution "abcdef" `shouldBe` ["ab", "cd", "ef"]
