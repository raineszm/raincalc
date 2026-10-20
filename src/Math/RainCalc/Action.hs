{-# LANGUAGE OverloadedRecordDot #-}

module Math.RainCalc.Action where

import Control.Monad.State.Class (state)
import Control.Monad.State.Strict (State, runState)
import Math.RainCalc.CalcError (CalcError)
import Math.RainCalc.Computation (Computation)
import Math.RainCalc.Output (Output)
import Math.RainCalc.RPNState (RPNState)

data SessionState = SessionState
    { rpn :: RPNState
    , activeModifier :: Maybe ModifierSpec
    }

sessionState :: RPNState -> SessionState
sessionState rpn = SessionState{rpn, activeModifier = Nothing}

type Session = State SessionState

zoomRPN :: Computation a -> Session a
zoomRPN comp = state $ \s ->
    let (a, r') = runState comp s.rpn
     in (a, s{rpn = r'})

data OperatorSpec = OperatorSpec
    { arity :: Int
    , runOp :: [Double] -> Either CalcError Double
    }

data ModifierSpec = ModifierSpec
    { runMod :: String -> Computation Output
    , runModSolo :: Computation Output
    }

data Action a = Action
    { names :: [String]
    , actionSpec :: a
    , desc :: String
    }

data ActionEntry
    = ECommand (Action (Computation Output))
    | EOperator (Action OperatorSpec)
    | EModifier (Action ModifierSpec)
