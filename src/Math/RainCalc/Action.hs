module Math.RainCalc.Action where

import Math.RainCalc.Computation (Computation)
import Math.RainCalc.Output (Output)

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
