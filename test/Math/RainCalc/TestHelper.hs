module Math.RainCalc.TestHelper where
import           Control.Monad.State.Strict (evalState, execState)
import           Math.RainCalc.RPNState

run :: Computation a -> RPNState
run c = execWithStack c []

runStack :: Computation a -> [Double]
runStack = stack_ . run

stackToState :: [Double] -> RPNState
stackToState stack = emptyState { stack_ = stack }

evalWithStack :: Computation a -> [Double] -> a
evalWithStack c = evalState c . stackToState

execWithStack :: Computation a -> [Double] -> RPNState
execWithStack c = execState c . stackToState

top :: Computation a -> Double
top c = head $ evalState (c >> pop 1) emptyState
