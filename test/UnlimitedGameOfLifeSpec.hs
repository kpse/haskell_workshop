module UnlimitedGameOfLifeSpec where
import UnlimitedGameOfLife (getGeneration, htmlize)

import Test.Hspec
import Test.HUnit

import Data.List (intercalate)

start = [[1,0,0],
         [0,1,1],
         [1,1,0]]
end   = [[0,1,0],
         [0,0,1],
         [1,1,1]]

spec = describe ("Glider<br>" ++ htmlize start) $ do
  it "Glider 1" $ do
    let actual = getGeneration start 1
        errorMsg = intercalate "<br>" ["expected:", htmlize end, "got:" , htmlize actual]

    assertBool errorMsg (actual == end)
