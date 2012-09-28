module Math.RainCalc.Numbers where
	import Data.Char (isHexDigit)
	import Text.Regex.PCRE
	
	import Math.RainCalc.State (State, push)

	handleNumber :: State -> String -> State
	handleNumber state num  = push state $ read num

	numRegex :: String
	numRegex = "^-?(\\d+\\.\\d*|\\d*\\.\\d+|\\d+)(e-?\\d+)?$"

	isNumber :: String -> Bool
	isNumber = (=~numRegex)
