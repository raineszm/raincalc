module Math.RainCalc.Modifier where
	import qualified Data.Map as Map
	import Data.Maybe (fromMaybe, isJust)
	import Control.Monad (liftM, join)

	import Math.RainCalc.State (State, modifier_)
	import Math.RainCalc.Operator (operators)
	import Math.RainCalc.Command (commands)
	import Math.RainCalc.Utils

	data Modifier = Modifier
		{action_ :: State -> String -> IO State
		,unary_ :: State -> IO State
		,help_ :: String}

	help :: Modifier
	help = Modifier
		{action_ = (\state _ -> return state)
		,unary_ = unaryHelpAction
		,help_ = "Displays help information"}

	unaryHelpAction :: State -> IO State
	unaryHelpAction state = do
		putStrLn "Available Commands: "
		mapM_ (putStr . (++"\n")) $ Map.keys operators
		putStrLn ""
		mapM_ (putStr . (++"\n")) $ Map.keys commands
		return state

	isModifier :: String -> Bool
	isModifier = (`Map.member`modifiers)

	modifiers :: Map.Map String Modifier
	modifiers = Map.fromList $ expand
		[([".?", ".h", ".help"], help)]

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