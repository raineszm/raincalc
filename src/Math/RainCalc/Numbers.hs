module Math.RainCalc.Numbers where
	import Math.RainCalc.State (State, push)

	handleNumber :: State -> Double -> State
	handleNumber = push
