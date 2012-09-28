module Math.RainCalc.Command where
	import qualified Data.Map as Map
	import System.Exit (exitSuccess)

	import Math.RainCalc.State (State, pop)
	import Math.RainCalc.Utils
	import Math.RainCalc.Classes

	data Command = Command
		{action_ :: State -> IO State
		,help_ :: String}

	instance Documented Command where
		getHelp = help_


	printStack :: Command
	printStack = Command
		{action_ = printStackAction
		,help_ = "Prints the stack to the screen."}

	printStackAction :: State -> IO State
	printStackAction state = do
		print state
		return state

	popStack :: Command
	popStack = Command
		{action_ = popStackAction
		,help_ = "Pops and presents the top of the stack."}

	popStackAction :: State -> IO State
	popStackAction state = do
			putStrLn top
			return newState
		where
			top = case popped of
				[] -> "Empty Stack."
				[x] -> show x
			(popped, newState) = pop state 1

	quitCommand :: Command
	quitCommand = Command
		{action_ = \_ -> exitSuccess
		,help_ = "Quits the program."}

	commands :: Map.Map String Command
	commands = Map.fromList $ expand
		[([".s"], printStack)
		,(["."], popStack)
		,([".q", ".quit"], quitCommand)]

	isCommand :: String -> Bool
	isCommand tok = Map.member tok commands

	handleCommand :: State -> String -> IO State
	handleCommand state tok = (action_ foundCmd) state
		where
			(Just foundCmd) = Map.lookup tok commands
