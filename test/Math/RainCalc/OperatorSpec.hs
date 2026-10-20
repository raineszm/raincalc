{-# LANGUAGE OverloadedRecordDot #-}

module Math.RainCalc.OperatorSpec (spec) where

import Control.Applicative ()
import Data.List (find)
import Data.Maybe (fromJust)
import Math.RainCalc.Action (Action (..), OperatorSpec)
import Math.RainCalc.Operator
import Math.RainCalc.TestHelper
import Test.Hspec
import Test.QuickCheck ()
import Text.Show.Functions ()

spec :: Spec
spec = describe "Operator" $ do
    argumentCountingSpec
    arithmeticSpec

getOp :: String -> OperatorSpec
getOp op = actionSpec . fromJust $ find matches operators
  where
    matches x = op `elem` x.names

argumentCountingSpec :: Spec
argumentCountingSpec =
    describe "Argument counting" $
        it "properly handles arity" $
            cases `shouldSatisfy` all test
  where
    test (stack, op, result) =
        runOnStack (evalOperator $ getOp op) stack == result
    cases =
        [ ([1, 2], "+", [3])
        , ([1, 2, 3, 4], "sum", [10])
        , ([1, 2, 3, 4], "prod", [24])
        ]

arithmeticSpec :: Spec
arithmeticSpec =
    describe "Arithmetic" $
        it "properly do arithmetic" $
            cases `shouldSatisfy` all test
  where
    test (stack, op, result) =
        runOnStack (evalOperator $ getOp op) stack == result
    cases =
        [ ([1, 2, 4], "+", [3, 4])
        , ([1, 2, 4], "-", [1, 4])
        , ([1, 2, 4], "*", [2, 4])
        , ([1, 2, 4], "/", [2, 4])
        , ([1, 2, 4], "neg", [-1, 2, 4])
        , ([2, 2, 3], "^", [4, 3])
        ]
