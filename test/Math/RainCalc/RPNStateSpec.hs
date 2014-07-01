module Math.RainCalc.RPNStateSpec (spec) where

import           Control.Applicative
import           Math.RainCalc.RPNState
import           Test.Hspec
import           Test.QuickCheck
import           Text.Show.Functions      ()

import           Math.RainCalc.TestHelper

spec :: Spec
spec = describe "RPNState" $ do
    pushSpec
    popSpec
    smapSpec
    dupSpec
    swapSpec
    sumSpec
    clearSpec

pushSpec :: Spec
pushSpec = describe "push" $
    it "should add the number to the stack" $ property $
        elem <$> id <*> runStack . push

popSpec :: Spec
popSpec = describe "push" $ do
    it "should return an empty list for an empty stack" $ property $
        null . runStack . pop
    it "should get the top of the stack" $ property $
        (==) <$> popOne . pure <*> pure
    where
        popOne = popN 1
        popN :: Int -> [Double] -> [Double]
        popN n = evalWithStack (pop n)

dupSpec :: Spec
dupSpec = describe "dup" $ do
    it "should be id on the empty stack" $ do
        runStack dup `shouldSatisfy` null

    it "should copy the top element of the stack" $ property $
        \stk-> not (null stk) ==>
            let (x:_) = stk in
            (stack_ . execWithStack dup) stk
            == x:stk

swapSpec :: Spec
swapSpec = describe "swap" $ do
    it "should be id on the empty stack" $ do
        runStack swap `shouldSatisfy` null

    it "should swap the two top elements of the stack" $ property $
        \stk-> (not . null . drop 1) stk ==>
            let (x:y:xs) = stk in
            (stack_ . execWithStack swap) stk
            == y:x:xs

smapSpec :: Spec
smapSpec = describe "smap" $ do
    it "should allow modification of the stack size" $ do
        stack_ (smap (1:) emptyState) `shouldBe` [1]

    it "should apply the given function to the stack" $ property $
        \f -> stack_ (smap f emptyState) == f []


sumSpec :: Spec
sumSpec = describe "sumStack" $ do
    it "should return 0 for the empty stack" $ do
        runStack sumStack `shouldBe` [0]

    it "should consume the stack and push its sum" $ property $
        (==) <$> (stack_ . execWithStack sumStack) <*> (return . sum)

clearSpec :: Spec
clearSpec = describe "clear" $ do
    it "should always give us an empty stack" $ property $
        null . stack_ . execWithStack clear
