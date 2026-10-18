{-# LANGUAGE OverloadedRecordDot #-}

module Math.RainCalc.Action where

import Control.Monad.State.Class (state)
import Control.Monad.State.Strict (State, runState)
import Math.RainCalc.Computation (Computation)
import Math.RainCalc.Output (Output)
import Math.RainCalc.RPNState (RPNState)

data SessionState = SessionState
    { rpn :: RPNState
    , activeModifier :: Maybe Modifier
    }

sessionState :: RPNState -> SessionState
sessionState rpn = SessionState{rpn, activeModifier = Nothing}

type Session = State SessionState

zoomRPN :: Computation a -> Session a
zoomRPN comp = state $ \s ->
    let (a, r') = runState comp s.rpn
     in (a, s{rpn = r'})

data Operator = Operator
    { arity :: Int
    , runOp :: [Double] -> Double
    }

data Modifier = Modifier
    { runMod :: String -> Computation Output
    , runModSolo :: Computation Output
    -- , help_ :: String
    }

data ActionEntry
    = ECommand (Computation Output)
    | EOperator Operator
    | EModifier Modifier
