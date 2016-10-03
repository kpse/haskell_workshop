module LastDigitSpec where
import LastDigit (lastDigit)
import Test.Hspec
import Test.QuickCheck

examples = do
  it "should work for some examples" $ do
    lastDigit []         `shouldBe` 1
    lastDigit [0,0]      `shouldBe` 1 -- 0 ^ 0
    lastDigit [0,0,0]    `shouldBe` 0 -- 0^(0 ^ 0) = 0^1 = 0
    lastDigit [0,0,1]    `shouldBe` 1
    lastDigit [2,0,0]    `shouldBe` 2
    lastDigit [0,0,2]    `shouldBe` 1
    lastDigit [2,2,0]    `shouldBe` 2
    lastDigit [10,0,0]    `shouldBe` 0
    lastDigit [0,10,1,0]    `shouldBe` 0
    lastDigit [7,10,0,0]    `shouldBe` 9
    lastDigit [2,10,1,0]    `shouldBe` 4
    lastDigit [1,2]      `shouldBe` 1
    lastDigit [3,4,5]    `shouldBe` 1
    lastDigit [4,3,6]    `shouldBe` 4
    lastDigit [7,6,21]   `shouldBe` 1
    lastDigit [12,30,21] `shouldBe` 6
    lastDigit [937640,767456,981242] `shouldBe` 0
    lastDigit [123232,694022,140249] `shouldBe` 6
    lastDigit [499942,898102,846073] `shouldBe` 6

simpleProperties = do
  it "lastDigit [x] == x `mod` 10" $
    property (\(NonNegative x) -> lastDigit [x] `shouldBe` x `mod` 10)
  it "lastDigit [x, y] == x ^ y `mod` 10" $
    property (\(NonNegative x) (NonNegative y) -> lastDigit [x, y] `shouldBe` x ^ y `mod` 10)

spec = describe "main" $ do
  describe "Examples" examples
  describe "Simple properties" simpleProperties
