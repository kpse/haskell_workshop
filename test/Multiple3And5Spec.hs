module Multiple3And5Spec
    where
import MultiplesOf3And5
import Test.Hspec
import Test.QuickCheck

spec = describe "solution" $ do
    it "testing 10" $ (solution 10) `shouldBe` 23
