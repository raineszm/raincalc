{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Math.RainCalc.RPNStateSpec (spec) where

import Control.Applicative ()
import Data.List (isInfixOf, mapAccumL)
import Math.RainCalc.RPNState
import Test.Hspec
import Test.QuickCheck
import Text.Show.Functions ()

spec :: Spec
spec = describe "RPNState" $ do
  pushSpec
  smapSpec
  dupSpec
  swapSpec
  clearSpec
  formatSpec

fromStack :: [Double] -> RPNState
fromStack stk = emptyState {stack_ = stk}

szoom :: (RPNState -> RPNState) -> [Double] -> [Double]
szoom f = stack_ . f . fromStack

onEmpty :: (RPNState -> RPNState) -> [Double]
onEmpty = flip szoom []

pushSpec :: Spec
pushSpec =
  describe "push" $
    it "should add the number to the stack" $
      property $
        elem <$> id <*> onEmpty . push_

dupSpec :: Spec
dupSpec = describe "dup" $ do
  it "should be id on the empty stack" $
    onEmpty dup_ `shouldSatisfy` null

  it "should copy the top element of the stack" $
    property $
      \stk ->
        not (null stk) ==>
          let (x : _) = stk
           in szoom dup_ stk
                == x : stk

swapSpec :: Spec
swapSpec = describe "swap" $ do
  it "should be id on the empty stack" $
    onEmpty swap_ `shouldSatisfy` null

  it "should swap the two top elements of the stack" $
    property $
      \stk ->
        (length stk > 1) ==>
          let (x : y : xs) = stk
           in szoom swap_ stk
                == y : x : xs

smapSpec :: Spec
smapSpec = describe "smap" $ do
  it "should allow modification of the stack size" $
    onEmpty (smap (1 :)) `shouldBe` [1]

  it "should apply the given function to the stack" $
    property $
      \f -> onEmpty (smap f) == f []

clearSpec :: Spec
clearSpec =
  describe "clear" $
    it "should always give us an empty stack" $
      property $
        null . szoom clear_

fmtEntryMatcher :: Int -> Double -> String -> Bool
fmtEntryMatcher i x = any ((&&) <$> strInfix i <*> strInfix x) . lines
  where
    strInfix :: (Show a) => a -> String -> Bool
    strInfix = isInfixOf . show

formatSpec :: Spec
formatSpec = describe "formatStack" $ do
  it "should display a special message for empty stack" $
    formatStack [] `shouldBe` "(EMPTY)"

  it "should display each entry with a position" $
    property $
      \stk ->
        (not . null) stk ==>
          let fmted = formatStack stk
              matcher i x = (i + 1, fmtEntryMatcher i x fmted)
           in and (snd $ mapAccumL matcher 0 stk)
