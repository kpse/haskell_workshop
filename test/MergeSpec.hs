module MergeSpec where
import Merge (merge)
import Test.Hspec

spec = describe "merge" $ do
    it "should work for the examples" $ do
      merge [1,2] [3]   `shouldBe` [1..3]
      merge [1,2] [3,4] `shouldBe` [1..4]
      merge [1] [2,3,4] `shouldBe` [1..4]
      merge [4] [3] `shouldBe` [3, 4]
    -- it "shouldn't use Data.List" $ do
    --   hidden [Module "Data.List", FromModule "Data.List" "sort"]
