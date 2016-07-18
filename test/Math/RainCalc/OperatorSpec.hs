module Math.RainCalc.OperatorSpec (spec) where

import           Control.Applicative      ()
import           Math.RainCalc.Operator
import           Test.Hspec
import           Test.QuickCheck          ()
import           Text.Show.Functions      ()

import           Math.RainCalc.TestHelper


spec :: Spec
spec = describe "Operator" $ do
  argumentCountingSpec
  arithmeticSpec

argumentCountingSpec :: Spec
argumentCountingSpec = describe "Argument counting" $
  it "properly handles arity" $
    cases `shouldSatisfy` all test
  where
    test (stack, op, result) =
      runOnStack (handleOperator op) stack == result
    cases =
      [
          ([1, 2],       "+",    [3])
        , ([1, 2, 3, 4], "sum",  [10])
        , ([1, 2, 3, 4], "prod", [24])
      ]

arithmeticSpec :: Spec
arithmeticSpec = describe "Arithmetic" $
  it "properly do arithmetic" $
    cases `shouldSatisfy` all test
  where
    test (stack, op, result) =
      runOnStack (handleOperator op) stack == result
    cases = [
           ([1, 2, 4], "+",   [3, 4])
         , ([1, 2, 4], "-",   [1, 4])
         , ([1, 2, 4], "*",   [2, 4])
         , ([1, 2, 4], "/",   [2, 4])
         , ([1, 2, 4], "neg", [-1, 2, 4])
         , ([2, 2, 3], "^",   [4, 3])
      ]
