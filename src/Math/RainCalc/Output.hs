module Math.RainCalc.Output where

import Control.Monad.State.Class (gets)
import Data.Functor (($>))
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

silent :: (Monad m) => m () -> m Output
silent = ($> Silent)

renderOutput :: Output -> Computation String
renderOutput Silent = pure ""
renderOutput (Message m) = pure m
renderOutput (Value i) = pure $ show i
renderOutput PrintStack = gets displayState
renderOutput PrintRegisters = ("REGISTERS:\n" ++) <$> gets present
renderOutput (ShowError e) = renderError e

renderError :: CalcError -> Computation String
renderError _ = return ""
