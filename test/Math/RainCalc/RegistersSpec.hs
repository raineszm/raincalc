module Math.RainCalc.RegistersSpec (spec) where

import Math.RainCalc.RPNState (emptyState)
import Math.RainCalc.Registers
import Test.Hspec
import Test.QuickCheck
import Text.Show.Functions ()

spec :: Spec
spec =
  describe
    "Registers"
    setGetIdentSpec

setGetIdentSpec :: Spec
setGetIdentSpec =
  it "get after set returns same value" $
    property setGetIdentProp

setGetIdentProp :: Char -> Double -> Property
setGetIdentProp r v =
  isValid r ==>
    all (== v) $
      getR_ r (setR_ r v emptyState)
