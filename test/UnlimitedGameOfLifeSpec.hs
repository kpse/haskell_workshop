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
end2  = [[1,0,1],
         [0,1,1],
         [0,1,0]]
start3  = [[1,1,1,0,0,0,1,0],
          [1,0,0,0,0,0,0,1],
          [0,1,0,0,0,1,1,1]]
end3  = [[1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1]
         ]

spec = describe ("Glider<br>" ++ htmlize start) $ do
  it "Glider 1" $ do
    let actual = getGeneration start 1
        errorMsg = intercalate "<br>" ["expected:", htmlize end, "got:" , htmlize actual]

    assertBool errorMsg (actual == end)
  it "Glider 2" $ do
      let actual = getGeneration start 2
          errorMsg = intercalate "<br>" ["expected:", htmlize end, "got:" , htmlize actual]

      assertBool errorMsg (actual == end2)
  it "Glider 100 " $ do
      let actual = getGeneration start3 10
          errorMsg = intercalate "<br>" ["expected:", htmlize end, "got:" , htmlize actual]

      assertBool errorMsg (actual == end3)
