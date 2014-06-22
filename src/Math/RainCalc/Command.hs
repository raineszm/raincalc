module Math.RainCalc.Command where
	import qualified Data.Map as Map
	import Control.Monad.State.Lazy
	import Control.Monad.Trans.Class (lift)

	import Math.RainCalc.RPNState (Computation,
		pop, dup, swap, clear, sumStack, displayState, terminate)
	import Math.RainCalc.Registers (present)
	import Math.RainCalc.Utils
	import Math.RainCalc.Classes

	data Command = Command
		{action_ :: Computation String
		,help_ :: String}

	instance Documented Command where
		getHelp = help_

	printStack :: Command
	printStack = Command
		{action_ = printStackAction
		,help_ = "Prints the stack to the screen."}

	printStackAction :: Computation String
	printStackAction = gets displayState

	popStack :: Command
	popStack = Command
		{action_ = popStackAction
		,help_ = "Pops and presents the top of the stack."}

	popStackAction :: Computation String
	popStackAction = do
			popped <- pop 1
			return $ case popped of
				[] -> "Empty Stack."
				[x] -> show x

	quitCommand :: Command
	quitCommand = Command
		{action_ = modify terminate >> return ""
		,help_ = "Quits the program."}

	dupCommand :: Command
	dupCommand = Command
		{action_ = dup >> return ""
		,help_ = "[ ... x ] -> [ ... x, x ]"}

	swapCommand :: Command
	swapCommand = Command
		{action_ = swap >> return ""
		,help_ = "[ ... x, y ] -> [ ... y, x ]"}

	clearCommand :: Command
	clearCommand = Command
		{action_ = clear >> return ""
		,help_ = "[ ... ] -> []"}

	sumCommand :: Command
	sumCommand = Command
		{action_ = sumStack >> return ""
		,help_ = "[ ... ] -> [sum(...)]"}

	printRegisters :: Command
	printRegisters = Command
		{action_ = printRegisterAction
		,help_ = "Prints the set registers to the screen."}

	printRegisterAction :: Computation String
	printRegisterAction = do
		presented <- gets present
		return $ "REGISTERS:\n" ++ presented

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

	handleCommand :: String -> Computation String
	handleCommand tok = action_ foundCmd
		where
			(Just foundCmd) = Map.lookup tok commands
