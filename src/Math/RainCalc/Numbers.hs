module Math.RainCalc.Numbers where
	import Text.Regex.PCRE

	import Math.RainCalc.RPNState (RPNState, push)

	handleNumber :: RPNState -> String -> RPNState
	handleNumber state num  = push state $ read num

	numRegex :: String
	numRegex = "^-?(\\d+\\.\\d*|\\d*\\.\\d+|\\d+)(e-?\\d+)?$"

	isNumber :: String -> Bool
	isNumber = (=~numRegex)
