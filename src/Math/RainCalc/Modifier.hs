module Math.RainCalc.Modifier where
	import qualified Data.Map as Map
	import Data.Maybe (fromMaybe, isJust, fromJust)
	import Control.Monad (liftM, join)
	import Data.List (intercalate)
	import System.Console.Haskeline (InputT, outputStrLn)

	import Math.RainCalc.State (State, modifier_)
	import Math.RainCalc.Operator (operators, isOperator)
	import Math.RainCalc.Command (commands, isCommand)
	import Math.RainCalc.Utils
	import Math.RainCalc.Classes

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
		outputStrLn $ wrapLine 70 $ combine $ Map.keys operators
		outputStrLn $ wrapLine 70 $ combine $ Map.keys commands
		outputStrLn $ wrapLine 70 $ combine $ Map.keys modifiers
		return state

		where
			combine = intercalate " "

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


	isModifier :: String -> Bool
	isModifier = (`Map.member`modifiers)

	modifiers :: Map.Map String Modifier
	modifiers = Map.fromList $ expand
		[(["?", ".?", ".h", ".help"], help)]

	armModifier :: State -> String -> State
	armModifier state modName = state { modifier_ = Just modName}

	triggerModifier :: State -> String -> InputT IO State
	triggerModifier state tok = (action_ modifier) cleanedState tok
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
			(Just aMod) -> (unary_ aMod) cleanedState
			Nothing -> return state
		where
			cleanedState = state { modifier_ = Nothing }
			modifier = join $ liftM (`Map.lookup` modifiers) mmod
			mmod = modifier_ state