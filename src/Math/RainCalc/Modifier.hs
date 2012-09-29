module Math.RainCalc.Modifier where
	import qualified Data.Map as Map
	import Data.Maybe (fromMaybe, isJust, fromJust)
	import Control.Monad (liftM, join)
	import Data.List (stripPrefix)
	import System.Console.Haskeline (InputT, outputStrLn)
	import Numeric (readHex)

	import Math.RainCalc.State (State, modifier_, push, pop)
	import Math.RainCalc.Operator (operators, isOperator)
	import Math.RainCalc.Command (commands, isCommand)
	import Math.RainCalc.Utils
	import Math.RainCalc.Classes
	import qualified Math.RainCalc.Registers as Reg

	data Modifier = Modifier
		{action_ :: State -> String -> InputT IO State
		,unary_ :: State -> InputT IO State
		,help_ :: String}

	instance Documented Modifier where
		getHelp = help_

	help :: Modifier
	help = Modifier
		{action_ = helpAction
		,unary_ = unaryHelpAction
		,help_ = "Displays help information"}

	unaryHelpAction :: State -> InputT IO State
	unaryHelpAction state = do
		outputStrLn "Available Commands: "
		outputStrLn $ wrapLine 60 $ unwords $ Map.keys operators
		outputStrLn $ wrapLine 60 $ unwords $ Map.keys commands
		outputStrLn $ wrapLine 60 $ unwords $ Map.keys modifiers
		return state

	helpAction :: State -> String -> InputT IO State
	helpAction state cmd = do
		helpAction' cmd
		return state

	helpAction' :: String -> InputT IO ()
	helpAction' cmd
		| isModifier cmd = printHelp modifiers cmd
		| isCommand cmd = printHelp commands cmd
		| isOperator cmd = printHelp operators cmd
		| otherwise = outputStrLn $ "Unknown command " ++ cmd

	printHelp :: (Documented v) => Map.Map String v -> String -> InputT IO ()
	printHelp objs key = do
		outputStrLn $ "Help for " ++ key ++ ":"
		outputStrLn $ getHelp obj
		where
			obj = fromJust $ Map.lookup key objs 

	hex :: Modifier
	hex = Modifier
		{action_ = hexAction
		,unary_ = unaryHexAction
		,help_ = "Read next input as a hex sequence."}

	unaryHexAction :: State -> InputT IO State
	unaryHexAction state = do
		outputStrLn "Invalid use of x:, no number supplied"
		return state

	hexAction :: State -> String -> InputT IO State
	hexAction state tok = newState
		where
			newState = if null rest
				then return $ push state num
				else do
					outputStrLn $ "Invalid hex sequence: " ++ tok
					return state
			((num, rest):junk) = readHex digits
			digits = fromMaybe tok $ stripPrefix "0x" tok

	toRegister :: Modifier
	toRegister = Modifier
		{action_ = toRegisterAction
		,unary_ = unaryRegisterAction
		,help_ = "Pops the top of the stack to the specified register."}

	unaryRegisterAction :: State -> InputT IO State
	unaryRegisterAction state = do
		outputStrLn "No register supplied."
		return state

	toRegisterAction :: State -> String -> InputT IO State
	toRegisterAction state [c]
		| Reg.isValid c = case popped of 
			[x] -> return $ Reg.set poppedState c x
			_ -> do
				outputStrLn "Error: Empty stack"
				return state
		| otherwise = do
			outputStrLn $ "Invalid register: " ++ [c]
			return state

		where
			(popped, poppedState) = pop state 1
	toRegisterAction state s = do
		outputStrLn $ "Invalid register name: " ++ s
		return state

	fromRegister :: Modifier
	fromRegister = Modifier
		{action_ = fromRegisterAction
		,unary_ = unaryRegisterAction
		,help_ = "Pushes the value of the specified register to the stack."}

	fromRegisterAction :: State -> String -> InputT IO State
	fromRegisterAction state [c]
		| Reg.isValid c = case fetched of
			(Just val) -> return $ push state val
			Nothing -> do
				outputStrLn $ "Tried to fetch unset register " ++ [c]
				return state
		| otherwise = do
			outputStrLn $ "Invalid register: " ++ [c]
			return state

		where
			fetched = Reg.get state c

	fromRegisterAction state s = do
		outputStrLn $ "Invalid register name: " ++ s
		return state

	isModifier :: String -> Bool
	isModifier = (`Map.member`modifiers)

	modifiers :: Map.Map String Modifier
	modifiers = Map.fromList $ expand
		[(["?", ".?", ".h", ".help"], help)
		,(["x:"], hex)
		,(["->"], toRegister)
		,(["<-"], fromRegister)]

	armModifier :: State -> String -> State
	armModifier state modName = state { modifier_ = Just modName}

	triggerModifier :: State -> String -> InputT IO State
	triggerModifier state = action_ modifier cleanedState 
		where
			cleanedState = state { modifier_ = Nothing }
			modifier = fromMaybe throwErr maybeModifier
			maybeModifier = join $ liftM (`Map.lookup` modifiers) mmod
			mmod = modifier_ state	
			throwErr = error "Somehow a modifier became undefined"

	activeModifier :: State -> Bool
	activeModifier = isJust . modifier_

	cleanupModifiers :: State -> InputT IO State
	cleanupModifiers state = case modifier of
			(Just aMod) -> unary_ aMod cleanedState
			Nothing -> return state
		where
			cleanedState = state { modifier_ = Nothing }
			modifier = join $ liftM (`Map.lookup` modifiers) mmod
			mmod = modifier_ state