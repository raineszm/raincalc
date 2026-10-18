module Math.RainCalc.Output where

import Control.Monad.State.Class (gets)
import Math.RainCalc.CalcError (CalcError)
import Math.RainCalc.Computation (Computation)
import Math.RainCalc.RPNState (displayState)
import Math.RainCalc.Registers (present)

data Output
    = Silent
    | Message String
    | Value Double
    | PrintStack
    | PrintRegisters
    | ShowError CalcError

silent :: Computation () -> Computation Output
silent c = c *> pure Silent

renderOutput :: Output -> Computation String
renderOutput Silent = return ""
renderOutput (Message m) = return m
renderOutput (Value i) = return $ show i
renderOutput PrintStack = gets displayState
renderOutput PrintRegisters = ("REGISTERS:\n" ++) <$> gets present

renderError :: CalcError -> Computation String
renderError _ = return ""
