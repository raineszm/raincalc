module Math.RainCalc.Modifier where
	import qualified Data.Map as Map
	import Data.Maybe (fromMaybe, isJust, fromJust)
	import Control.Monad (liftM, join)
	import Data.List (intercalate)

	import Math.RainCalc.State (State, modifier_)
	import Math.RainCalc.Operator (operators, isOperator)
	import Math.RainCalc.Command (commands, isCommand)
	import Math.RainCalc.Utils
	import Math.RainCalc.Classes

	data Modifier = Modifier
		{action_ :: State -> String -> IO State
		,unary_ :: State -> IO State
		,help_ :: String}

	instance Documented Modifier where
		getHelp = help_

	help :: Modifier
	help = Modifier
		{action_ = helpAction
		,unary_ = unaryHelpAction
		,help_ = "Displays help information"}

	unaryHelpAction :: State -> IO State
	unaryHelpAction state = do
		putStrLn "Available Commands: "
		putStrLn $ wrapLine 70 $ combine $ Map.keys operators
		putStrLn $ wrapLine 70 $ combine $ Map.keys commands
		putStrLn $ wrapLine 70 $ combine $ Map.keys modifiers
		return state

		where
			combine = intercalate " "

	helpAction :: State -> String -> IO State
	helpAction state cmd = do
		helpAction' cmd
		return state

	helpAction' :: String -> IO ()
	helpAction' cmd
		| isModifier cmd = printHelp modifiers cmd
		| isCommand cmd = printHelp commands cmd
		| isOperator cmd = printHelp operators cmd
		| otherwise = putStrLn $ "Unknown command " ++ cmd

	printHelp :: (Documented v) => Map.Map String v -> String -> IO ()
	printHelp objs key = do
		putStrLn $ "Help for " ++ key ++ ":"
		putStrLn $ getHelp obj
		where
			obj = fromJust $ Map.lookup key objs 


	isModifier :: String -> Bool
	isModifier = (`Map.member`modifiers)

	modifiers :: Map.Map String Modifier
	modifiers = Map.fromList $ expand
		[(["?", ".?", ".h", ".help"], help)]

	armModifier :: State -> String -> State
	armModifier state modName = state { modifier_ = Just modName}

	triggerModifier :: State -> String -> IO State
	triggerModifier state tok = (action_ modifier) cleanedState tok
		where
			cleanedState = state { modifier_ = Nothing }
			modifier = fromMaybe throwErr maybeModifier
			maybeModifier = join $ liftM (`Map.lookup` modifiers) mmod
			mmod = modifier_ state	
			throwErr = error "Somehow a modifier became undefined"

	activeModifier :: State -> Bool
	activeModifier = isJust . modifier_

	cleanupModifiers :: State -> IO State
	cleanupModifiers state = case modifier of
			(Just aMod) -> (unary_ aMod) cleanedState
			Nothing -> return state
		where
			cleanedState = state { modifier_ = Nothing }
			modifier = join $ liftM (`Map.lookup` modifiers) mmod
			mmod = modifier_ state