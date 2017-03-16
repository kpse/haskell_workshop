module Multiple3And5Spec
    where
import MultiplesOf3And5
import Test.Hspec

spec = describe "solution" $
    it "testing 10" $
      solution 10 `shouldBe` 23
