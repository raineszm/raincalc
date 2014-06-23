module Math.RainCalc.TestHelper where
import           Control.Monad.State.Lazy (evalState, execState)
import           Math.RainCalc.RPNState

run :: Computation a -> RPNState
run c = execState c emptyState

runStack :: Computation a -> [Double]
runStack = stack_ . run

stackToState :: [Double] -> RPNState
stackToState stack = emptyState { stack_ = stack }

withStack :: Computation a -> [Double] -> a
withStack c = evalState c . stackToState

top :: Computation a -> Double
top c = head $ evalState (c >> pop 1) emptyState

