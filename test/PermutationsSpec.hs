module PermutationsSpec where
import Permutations (permutations)
import Test.Hspec

spec = describe "permutations" $ do
    it "should work for some examples" $ do
      permutations    "a" `shouldBe` ["a"]
      permutations   "ab" `shouldBe` ["ab", "ba"]
      permutations "aabb" `shouldBe` ["aabb","abab","abba","baab","baba","bbaa"]

    -- it "should not use built-in methods" $
    --   hidden [FromModule "Data.List" "permutations"]
