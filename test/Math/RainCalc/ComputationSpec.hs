module Math.RainCalc.ComputationSpec (spec) where
import           Control.Applicative       ()
import           Math.RainCalc.Computation
import           Test.Hspec
import           Test.QuickCheck
import           Text.Show.Functions       ()

import           Math.RainCalc.TestHelper

spec :: Spec
spec = describe "Computation" popSpec

popSpec :: Spec
popSpec = describe "pop" $ do
    it "should return an empty list for an empty stack" $ property $
        null . runStack . pop
    it "should get the top of the stack" $ property $
        (==) <$> popOne . pure <*> pure
    where
        popOne = popN 1
        popN :: Int -> [Double] -> [Double]
        popN n = evalWithStack (pop n)
