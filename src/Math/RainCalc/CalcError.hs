module Math.RainCalc.CalcError (CalcError (..)) where

data CalcError
  = MalformedInput String
  | InvalidRegister String
  | EmptyStack
