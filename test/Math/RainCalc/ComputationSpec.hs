module Math.RainCalc.ComputationSpec (spec) where

import Control.Applicative ()
import Math.RainCalc.Computation
import Math.RainCalc.TestHelper
import Test.Hspec
import Test.QuickCheck
import Text.Show.Functions ()

spec :: Spec
spec = describe "Computation" popSpec

popSpec :: Spec
popSpec = describe "pop" $ do
  it "should return an empty list for an empty stack" $
    property $
      null . runStack . pop
  it "should get the top of the stack" $
    property popTopProp

popTopProp :: Int -> [Double] -> Property
popTopProp n lst =
  length lst >= n ==>
    evalWithStack (pop n) lst `shouldBe` take n lst
