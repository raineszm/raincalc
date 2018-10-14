module Math.RainCalc.Numbers
  ( handleNumber
  , isNumber
  ) where

import           Text.Regex.PCRE

import           Math.RainCalc.Computation (Computation, push)

handleNumber :: String -> Computation ()
handleNumber = push . read'

read' :: String -> Double
read' num@('.':_) = read' ('0' : num)
read' x           = read x

numRegex :: String
numRegex = "^-?(\\d+\\.\\d*|\\d*\\.\\d+|\\d+)(e-?\\d+)?$"

isNumber :: String -> Bool
isNumber = (=~ numRegex)
