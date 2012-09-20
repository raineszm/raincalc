module Math.RainCalc.Engine where
	import Control.Monad (foldM)

	import qualified Math.RainCalc.State as S
	import qualified Math.RainCalc.Operator as Op
	import qualified Math.RainCalc.Numbers as N
	import qualified Math.RainCalc.Command as Cmd

	processLine :: S.State -> String -> IO S.State
	processLine state line = foldM processToken state $ words line

	processToken :: S.State -> String -> IO S.State
	processToken state tok
		| N.isNumber tok = return $ N.handleNumber state tok
		| Op.isOperator tok = tryOperator state tok
		| Cmd.isCommand tok = Cmd.handleCommand state tok
		| otherwise = return state

	tryOperator :: S.State -> String -> IO S.State
	tryOperator state tok = case result of
		(Left msg) -> do
			putStrLn msg
			return state
		(Right newState) -> return newState
		where
			result = Op.handleOperator state tok