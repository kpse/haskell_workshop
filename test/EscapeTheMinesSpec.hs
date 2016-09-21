module EscapeTheMinesSpec where
import EscapeTheMines (Move(..), solve)
import Test.Hspec

spec = describe "Main" $ do
  describe "A trivial map (1x1)" $ do
    let map = [[True]];
    it "Should return an empty list, since we're already at the goal" $ do
      solve map (0,0) (0,0) `shouldBe` []

  describe "A pretty simple map (2x2)" $ do
    let map = unmap [" #"
                    ,"  "
                    ]
    it "Should return the only correct move" $ do
      solve map (0,0) (1,0) `shouldBe` [R]

    it "Should return the only moves necessary" $ do
      solve map (0,0) (1,1) `shouldBe` [R, D]


  describe "A linear map(1x4)" $ do
    let map = unmap [" "
                    ," "
                    ," "
                    ," "
                    ]

    it "Should return a chain of moves to the right" $ do
      solve map (0,0) (3,0) `shouldBe` [R, R, R]

    it "Should return a chain of moves to the left" $ do
      solve map (3,0) (0,0) `shouldBe` [L, L, L]

  describe "Should walk around an obstacle (3x3 map)" $ do
    let map = unmap ["   "
                    ,"## "
                    ,"   "
                    ]

    it "Should return the right sequence of moves" $ do
      solve map (0,0) (2,0) `shouldBe` [D, D, R, R, U, U]

  describe "Should be able to change directions multiple times (5x5 map)" $ do
    let map = unmap ["  ###"
                    ,"#  ##"
                    ,"##  #"
                    ,"###  "
                    ,"#### "
                    ]

    it "Should return a step sequence of moves" $ do
      solve map (0,0) (4,4) `shouldBe` [D, R, D, R, D, R, D, R]

  describe "Should avoid dead-ends (5x5 map)" $ do
    let map = unmap ["   ##"
                    ,"## # "
                    ,"     "
                    ," # ##"
                    ,"#    "
                    ]

    it "Should return the right moves" $ do
      solve map (0,0) (4,4) `shouldBe` [D, D, R, R, R, R, D, D]

unmap = map (map (== ' '))
