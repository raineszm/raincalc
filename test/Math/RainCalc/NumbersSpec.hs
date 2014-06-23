module Math.RainCalc.NumbersSpec (spec) where

  import Test.Hspec
  import Test.QuickCheck
  import Math.RainCalc.Numbers

  import Math.RainCalc.TestHelper

  spec :: Spec
  spec = describe "Numbers" $ do
    isNumberSpec
    handleNumberSpec

  isNumberSpec :: Spec
  isNumberSpec = describe "isNumber" $ do

    it "integers are numbers" $ property
      ((isNumber . show) :: Int -> Bool)

    it "doubles are numbers" $ property
      ((isNumber . show) :: Double -> Bool)

    it "should not match non-numbers" $
      map isNumber
        ["apple"
        ,"f"
        ,"1.1."
        ,"1..0"
        ,"1.0."
        ,"."
        ,"-."
        ,".e4"] `shouldSatisfy` not . or


  handleNumberSpec :: Spec
  handleNumberSpec = describe "handleNumber" $

    it "should push the number to the stack" $ property
      ((\n -> (top . handleNumber . show) n == n) :: Double -> Bool)