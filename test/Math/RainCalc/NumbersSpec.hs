module Math.RainCalc.NumbersSpec (spec) where

  import Test.Hspec
  import Test.QuickCheck
  import Math.RainCalc.Numbers

  spec :: Spec
  spec = describe "Numbers" $ do
    isNumberSpec

  isNumberSpec :: Spec
  isNumberSpec = describe "isNumber" $ do
    it "integers are numbers" $ property $
      ((isNumber . show) :: Int -> Bool)
    it "doubles are numbers" $ property $
      ((isNumber . show) :: Double -> Bool)



