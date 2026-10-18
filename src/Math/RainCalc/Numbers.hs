module Math.RainCalc.Numbers (
    handleNumber,
    isNumber,
)
where

import Math.RainCalc.Computation (Computation, push)
import Text.Regex.TDFA

handleNumber :: String -> Computation ()
handleNumber = push . read'

read' :: String -> Double
read' num@('.' : _) = read' ('0' : num)
read' x = read x

numRegex :: String
numRegex = "^-?([[:digit:]]+\\.[[:digit:]]*|[[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)(e-?[[:digit:]]+)?$"

isNumber :: String -> Bool
isNumber = (=~ numRegex)
