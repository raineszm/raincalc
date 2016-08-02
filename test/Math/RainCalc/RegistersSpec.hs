module Math.RainCalc.RegistersSpec (spec) where

import           Control.Applicative      ()
import           Math.RainCalc.Registers
import           Test.Hspec
import           Test.QuickCheck
import           Text.Show.Functions      ()

import           Math.RainCalc.TestHelper

spec :: Spec
spec = describe "Registers"
  setGetIdentSpec

setGetIdentSpec :: Spec
setGetIdentSpec = it "get after set returns same value" $
  property setGetIdentProp


setGetIdentProp :: Char -> Double -> Property
setGetIdentProp r v = isValid r ==> evalWithStack comp []
  where
    comp = all (==v) <$> (setR r v >> getR r)
