module TitleCaseSpec where

import           TitleCase
import           Test.Hspec

spec = describe "TitleCase" $
    it "change case" $ do
      titleCase "a an the of" "a clash of KINGS" `shouldBe` "A Clash of Kings"
      titleCase "The In" "THE WIND IN THE WILLOWS" `shouldBe` "The Wind in the Willows"
      titleCase "" "the quick brown fox" `shouldBe` "The Quick Brown Fox"
