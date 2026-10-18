{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Math.RainCalc.EngineSpec (spec) where

import Control.Applicative ()
import Data.List (isInfixOf)
import Math.RainCalc.Engine
import Math.RainCalc.TestHelper
import Test.Hspec
import Test.QuickCheck
import Text.Show.Functions ()

spec :: Spec
spec = describe "Engine" $ do
    processTokenSpec
    processLineSpec

processTokenSpec :: Spec
processTokenSpec = describe "processToken" $ do
    runCommand
    pushNumbers
    runOperator

runCommand :: Spec
runCommand =
    it "should trigger commands" $
        property ((`shouldSatisfy` null) . runOnStack (toComp session))
  where
    session = processToken "clear"

runOperator :: Spec
runOperator = it "should trigger operators" $
    property $ \stk ->
        (length stk > 1) ==>
            let (x : y : xs) = stk
             in runOnStack (toComp session) stk `shouldBe` (x + y) : xs
  where
    session = processToken "+"

pushNumbers :: Spec
pushNumbers =
    it "should push numbers" $
        property
            ( \m ->
                runOnStack (toComp $ processToken (show m)) [] `shouldBe` [m]
            )

processLineSpec :: Spec
processLineSpec = describe "processLine_" $ do
    runModifier
    listRegisters

runModifier :: Spec
runModifier =
    it "should trigger modifier on line end" $
        runStack (toComp session) `shouldBe` [10]
  where
    session = processLine_ "x: A"

listRegisters :: Spec
listRegisters =
    it "should allow display registers" $
        evalSessionWithStack session [] `shouldSatisfy` test
  where
    session = processLine_ "3 -> a .r"
    test = isInfixOf "3"
