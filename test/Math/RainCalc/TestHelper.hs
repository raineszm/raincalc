module Math.RainCalc.TestHelper where

import Control.Monad.State.Class (state)
import Control.Monad.State.Strict (evalState, execState, runState)
import Math.RainCalc.Action (Session, SessionState (..), sessionState)
import Math.RainCalc.Computation
import Math.RainCalc.RPNState

runStack :: Computation a -> [Double]
runStack = flip runOnStack []

runOnStack :: Computation a -> [Double] -> [Double]
runOnStack c = stack_ . execWithStack c

stackToState :: [Double] -> RPNState
stackToState stack = emptyState{stack_ = stack}

evalWithStack :: Computation a -> [Double] -> a
evalWithStack c = evalState c . stackToState

execWithStack :: Computation a -> [Double] -> RPNState
execWithStack c = execState c . stackToState

evalSessionWithStack :: Session a -> [Double] -> a
evalSessionWithStack c = evalState c . sessionState . stackToState

top :: Computation a -> Double
top c = head $ evalState (c >> pop 1) emptyState

toComp :: Session a -> Computation a
toComp session = state $ \r ->
    rpn <$> runState session (SessionState{rpn = r, activeModifier = Nothing})
