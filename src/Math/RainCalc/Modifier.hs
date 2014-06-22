module Math.RainCalc.Modifier where
	import qualified Data.Map as Map
	import Data.Maybe (fromMaybe, isJust, fromJust, isNothing)
	import Control.Monad (liftM, join, (>=>))
	import Control.Monad.State.Lazy (gets, modify)
	import Data.List (stripPrefix)
	import Numeric (readHex)

	import Math.RainCalc.RPNState (Computation, modifier_, push, pop)
	import Math.RainCalc.Operator (operators, isOperator)
	import Math.RainCalc.Command (commands, isCommand)
	import Math.RainCalc.Utils
	import Math.RainCalc.Classes
	import qualified Math.RainCalc.Registers as Reg

	data Modifier = Modifier
		{action_ :: String -> Computation String
		,unary_ :: Computation String
		,help_ :: String}

	instance Documented Modifier where
		getHelp = help_

	help :: Modifier
	help = Modifier
		{action_ = helpAction
		,unary_ = unaryHelpAction
		,help_ = "Displays help information"}

	unaryHelpAction :: Computation String
	unaryHelpAction = (return . unlines) outLines
		where
			outLines = header : map doWrap
				[Map.keys operators
				,Map.keys commands
				,Map.keys modifiers]
			header = "Available Commands: "
			doWrap = (wrapLine 60) . unwords

	helpAction ::String -> Computation String
	helpAction cmd
		| isModifier cmd = return $ formatHelp cmd modifiers
		| isCommand cmd = return $ formatHelp cmd commands
		| isOperator cmd = return $ formatHelp cmd operators
		| otherwise = return $ "Unknown command " ++ cmd

	formatHelp :: (Documented v) => String -> Map.Map String v -> String
	formatHelp key objs = "Help for " ++ key ++ ":\n" ++ getHelp obj
		where
			obj = fromJust $ Map.lookup key objs

	hex :: Modifier
	hex = Modifier
		{action_ = hexAction
		,unary_ = return "Invalid use of x:, no number supplied"
		,help_ = "Read next input as a hex sequence."}

	hexAction :: String -> Computation String
	hexAction tok = if null rest
			then push num >> return ""
			else return $ "Invalid hex sequence: " ++ tok
		where
			((num, rest):_) = readHex digits
			digits = fromMaybe tok $ stripPrefix "0x" tok

	toRegister :: Modifier
	toRegister = Modifier
		{action_ = toRegisterAction
		,unary_ = unaryRegisterAction
		,help_ = "Pops the top of the stack to the specified register."}

	unaryRegisterAction :: Computation String
	unaryRegisterAction = return "No register supplied."

	toRegisterAction :: String -> Computation String
	toRegisterAction [c]
		| Reg.isValid c = do
			popped <- pop 1
			case popped of
				[x] -> Reg.setR c x >> return ""
				_ -> return "Error: Empty stack"
		| otherwise = return $ "Invalid register: " ++ [c]

	toRegisterAction s = return $ "Invalid register name: " ++ s

	fromRegister :: Modifier
	fromRegister = Modifier
		{action_ = fromRegisterAction
		,unary_ = unaryRegisterAction
		,help_ = "Pushes the value of the specified register to the stack."}

	fromRegisterAction :: String -> Computation String
	fromRegisterAction [c]
		| Reg.isValid c = do
			fetched <- Reg.getR c
			if isNothing fetched
				then return ("Tried to fetch unset register " ++ [c])
				else push (fromJust fetched) >> return ""
		| otherwise = return $ "Invalid register: " ++ [c]

	fromRegisterAction s = return $ "Invalid register name: " ++ s

	isModifier :: String -> Bool
	isModifier = (`Map.member`modifiers)

	modifiers :: Map.Map String Modifier
	modifiers = Map.fromList $ expand
		[(["?", ".?", ".h", ".help"], help)
		,(["x:"], hex)
		,(["->"], toRegister)
		,(["<-"], fromRegister)]


	armModifier :: String -> Computation ()
	armModifier modName = modify $ \state -> state { modifier_ = Just modName}

	disarmModifier :: Computation ()
	disarmModifier = modify $ \state -> state { modifier_ = Nothing}

	activeModifier :: Computation Bool
	activeModifier = gets $ isJust . modifier_

	triggerModifier :: String -> Computation String
	triggerModifier tok = do
		mmod <- gets $ modifier_ >=> (`Map.lookup` modifiers)
		disarmModifier
		if isNothing mmod
			then return "Somehow a modifier became undefined"
			else action_ (fromJust mmod) tok

	cleanupModifiers :: Computation String
	cleanupModifiers = do
		mmod <- gets $ modifier_ >=> (`Map.lookup` modifiers)
		disarmModifier
		if isJust mmod
			then unary_ $ fromJust mmod
			else return ""