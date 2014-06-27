module Math.RainCalc.RPNStateSpec (spec) where

import           Math.RainCalc.RPNState
import           Test.Hspec
import           Test.QuickCheck

import           Math.RainCalc.TestHelper

spec :: Spec
spec = describe "RPNState" $ do
    pushSpec
    popSpec
    smapSpec
    dupSpec
    swapSpec

pushSpec :: Spec
pushSpec = describe "push" $
    it "should add the number to the stack" $ property $
        \x -> x `elem` runStack (push x)

popSpec :: Spec
popSpec = describe "push" $ do
    it "should return an empty list for an empty stack" $ property $
        null . runStack . pop
    it "should get the top of the stack" $ property $
        \x -> popOne [x] == [x]
    where
        popOne = popN 1
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
    it "should apply the given function to the stack" $ do
        stack_ (smap ((:)1) emptyState) `shouldBe` [1]