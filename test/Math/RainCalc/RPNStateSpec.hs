module Math.RainCalc.RPNStateSpec (spec) where

import           Control.Applicative
import           Data.List                (isInfixOf, mapAccumL)
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
    clearSpec
    formatSpec

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


clearSpec :: Spec
clearSpec = describe "clear" $ do
    it "should always give us an empty stack" $ property $
        null . stack_ . execWithStack clear

fmtEntryMatcher :: Int -> Double -> String -> Bool
fmtEntryMatcher i x = any ((&&) <$> strInfix i <*> strInfix x) . lines
    where
        strInfix :: (Show a) => a -> String -> Bool
        strInfix = isInfixOf.show

formatSpec :: Spec
formatSpec = describe "formatStack" $ do
    it "should display a special message for empty stack" $ do
        formatStack [] `shouldBe` "(EMPTY)"

    it "should display each entry with a position" $ property $
        \stk -> (not . null) stk ==>
            let fmted = formatStack stk
                matcher i x = (i + 1, fmtEntryMatcher i x fmted)
            in all id (snd $ mapAccumL matcher 0 stk)


