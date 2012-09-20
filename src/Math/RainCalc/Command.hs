module Math.RainCalc.Command where
	import qualified Data.Map as Map

	import Math.RainCalc.State (State)

	data Command = Command
		{action_ :: State -> IO State
		,help_ :: String}


	printStack :: Command
	printStack = Command
		{action_ = printStackAction
		,help_ = "Prints the stack to the screen"}

	printStackAction :: State -> IO State
	printStackAction state = do
		print state
		return state

	expandCommands :: [([String], Command)] -> [(String, Command)]
	expandCommands = concat . map alias
		where
			alias (names, cmd) = [(name, cmd) | name <- names]

	commands :: Map.Map String Command
	commands = Map.fromList $ expandCommands
		[([".s"], printStack)]

	isCommand :: String -> Bool
	isCommand tok = Map.member tok commands

	handleCommand :: State -> String -> IO State
	handleCommand state tok = (action_ foundCmd) state
		where
			(Just foundCmd) = Map.lookup tok commands
