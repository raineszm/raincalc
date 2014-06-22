module Math.RainCalc.Numbers where
	import Text.Regex.PCRE

	import Math.RainCalc.RPNState (Computation, push)

	handleNumber :: String -> Computation ()
	handleNumber num  = push $ read num

	numRegex :: String
	numRegex = "^-?(\\d+\\.\\d*|\\d*\\.\\d+|\\d+)(e-?\\d+)?$"

	isNumber :: String -> Bool
	isNumber = (=~numRegex)
