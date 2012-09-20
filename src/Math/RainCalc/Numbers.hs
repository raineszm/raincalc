module Math.RainCalc.Numbers where
	import Data.Char (isHexDigit)
	
	import Math.RainCalc.State (State, push)

	handleNumber :: State -> String -> State
	handleNumber state num  = push state $ read num

	isNumber :: String -> Bool
	isNumber = all isHexDigit
