module Math.RainCalc.Engine where
	import Control.Monad (foldM, liftM)
	import Control.Monad.State.Lazy (runState, execState)
	import System.Console.Haskeline (InputT, getInputLine, outputStrLn)
	import Data.Char (toLower)

	import qualified Math.RainCalc.RPNState as S
	import qualified Math.RainCalc.Operator as Op
	import qualified Math.RainCalc.Numbers as N
	import qualified Math.RainCalc.Command as Cmd
	import qualified Math.RainCalc.Modifier as Mod

	mainLoop :: S.RPNState -> InputT IO S.RPNState
	mainLoop state = do
		line <- getInputLine $ prompt state
		return state
		--case line of
		--	Nothing -> return state
		--	(Just jline) -> do
		--		newState <- processLine state $ map toLower jline
		--		mainLoop newState


	prompt :: S.RPNState -> String
	prompt state = "raincalc[s:" ++ size ++ "]> "
		where
			size = show $ S.stackSize state


	--processLine :: S.RPNState -> String -> InputT IO S.RPNState
	--processLine state line = foldM processToken state (words line) >>= Mod.cleanupModifiers

	--processToken :: String -> InputT IO (S.Computation ())
	--processToken tok = do

	doOutput :: S.Computation String -> S.RPNState -> InputT IO S.RPNState
	doOutput comp oldState = do
		let (out, newState) = runState comp oldState
		outputStrLn out
		return newState

	doComputation :: S.Computation () -> S.RPNState -> InputT IO S.RPNState
	doComputation comp =  return . (execState comp)


	processToken_ :: S.RPNState -> String -> InputT IO S.RPNState
	processToken_ state tok
		| Mod.isModifier tok = doComputation (Mod.armModifier tok) state
		| Op.isOperator tok = doComputation (Op.handleOperator tok) state
		| Cmd.isCommand tok = doOutput (Cmd.handleCommand tok) state
		| N.isNumber tok = doComputation (N.handleNumber tok) state
		| otherwise = do
			outputStrLn $ "Unrecognized sequence \"" ++ tok ++  "\""
			return state