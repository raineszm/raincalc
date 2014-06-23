module Math.RainCalc.RPNStateSpec (spec) where

import           Math.RainCalc.RPNState
import           Test.Hspec
import           Test.QuickCheck

import           Math.RainCalc.TestHelper

spec :: Spec
spec = describe "RPNState" $ do
	pushSpec
	popSpec

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
		popN n = withStack (pop n)
