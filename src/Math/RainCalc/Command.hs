module Math.RainCalc.Command where
	import qualified Data.Map as Map
	import System.Exit (exitSuccess)
	import System.Console.Haskeline (InputT, outputStrLn, outputStr)
	import Control.Monad.Trans.Class (lift)

	import Math.RainCalc.State (State, pop, dup, swap, clear)
	import Math.RainCalc.Registers (present)
	import Math.RainCalc.Utils
	import Math.RainCalc.Classes

	data Command = Command
		{action_ :: State -> InputT IO State
		,help_ :: String}

	instance Documented Command where
		getHelp = help_

	printStack :: Command
	printStack = Command
		{action_ = printStackAction
		,help_ = "Prints the stack to the screen."}

	printStackAction :: State -> InputT IO State
	printStackAction state = do
		outputStr $ show state
		return state


	popStack :: Command
	popStack = Command
		{action_ = popStackAction
		,help_ = "Pops and presents the top of the stack."}

	popStackAction :: State -> InputT IO State
	popStackAction state = do
			outputStrLn top
			return newState
		where
			top = case popped of
				[] -> "Empty Stack."
				[x] -> show x
			(popped, newState) = pop state 1

	quitCommand :: Command
	quitCommand = Command
		{action_ = \_ -> lift exitSuccess
		,help_ = "Quits the program."}

	dupCommand :: Command
	dupCommand = Command
		{action_ = return . dup
		,help_ = "[ ... x ] -> [ ... x, x ]"}

	swapCommand :: Command
	swapCommand = Command
		{action_ = return . swap
		,help_ = "[ ... x, y ] -> [ ... y, x ]"}

	clearCommand :: Command
	clearCommand = Command
		{action_ = return . clear
		,help_ = "[ ... ] -> []"}

	printRegisters :: Command
	printRegisters = Command
		{action_ = printRegisterAction
		,help_ = "Prints the set registers to the screen."}

	printRegisterAction :: State -> InputT IO State
	printRegisterAction state = do
		outputStrLn "REGISTERS:\n"
		outputStr $ present state
		return state


	commands :: Map.Map String Command
	commands = Map.fromList $ expand
		[([".s"], printStack)
		,(["."], popStack)
		,([".q", ".quit", "q", "quit"], quitCommand)
		,(["dup"], dupCommand)
		,(["swap"], swapCommand)
		,(["clear"], clearCommand)
		,([".r", ".reg", "registers"], printRegisters)]

	isCommand :: String -> Bool
	isCommand tok = Map.member tok commands

	handleCommand :: State -> String -> InputT IO State
	handleCommand state tok = action_ foundCmd state
		where
			(Just foundCmd) = Map.lookup tok commands
