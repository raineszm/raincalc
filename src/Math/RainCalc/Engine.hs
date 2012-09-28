module Math.RainCalc.Engine where
	import Control.Monad (foldM, liftM)
	import System.IO (hFlush, stdout)

	import qualified Math.RainCalc.State as S
	import qualified Math.RainCalc.Operator as Op
	import qualified Math.RainCalc.Numbers as N
	import qualified Math.RainCalc.Command as Cmd
	import qualified Math.RainCalc.Modifier as Mod

	mainLoop :: S.State -> IO S.State
	mainLoop state = do
		putPrompt state
		hFlush stdout
		line <- getLine
		newState <- processLine state line
		mainLoop newState


	putPrompt :: S.State -> IO ()
	putPrompt state = putStr $ "raincalc[s:" ++ size ++ "] "
		where
			size = show $ S.stackSize state


	processLine :: S.State -> String -> IO S.State
	processLine state line = (foldM processToken state $ words line) >>= Mod.cleanupModifiers

	processToken :: S.State -> String -> IO S.State
	processToken state tok
		| Mod.activeModifier state = Mod.triggerModifier state tok
		| Mod.isModifier tok = return $ Mod.armModifier state tok
		| Op.isOperator tok = return $ Op.handleOperator state tok
		| Cmd.isCommand tok = Cmd.handleCommand state tok
		| N.isNumber tok = return $ N.handleNumber state tok
		| otherwise = return state
