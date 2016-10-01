module FibonacciSpec where
import Test.Hspec
import Fibonacci

spec = describe "main" $ do
  describe "Basic tests" $ do
    it "fib 0" $ fib 0 `shouldBe` 0
    it "fib 1" $ fib 1 `shouldBe` 1
    it "fib 2" $ fib 2 `shouldBe` 1
    it "fib 3" $ fib 3 `shouldBe` 2
    it "fib 4" $ fib 4 `shouldBe` 3
    it "fib 5" $ fib 5 `shouldBe` 5

  describe "Negative values" $ do
    it "fib -6" $ fib (-6) `shouldBe` -8
    it "fib -6" $ fib (-7) `shouldBe` 13
    it "fib -96" $ fib (-96) `shouldBe` -51680708854858323072

  describe "Larger values" $ do
    it "fib 1000" $ fib 1000 `shouldBe` 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
    it "fib 2000" $ fib 2000 `shouldBe` 4224696333392304878706725602341482782579852840250681098010280137314308584370130707224123599639141511088446087538909603607640194711643596029271983312598737326253555802606991585915229492453904998722256795316982874482472992263901833716778060607011615497886719879858311468870876264597369086722884023654422295243347964480139515349562972087652656069529806499841977448720155612802665404554171717881930324025204312082516817125
    it "fib 100002" $ fib 100002 `shouldBe` fib 100000 + fib 100001
    it "fib 100003" $ fib 100001 `shouldBe` fib 100000 + fib 99999