module Math.RainCalc.Operator where
	import qualified Data.Map as Map

	import Math.RainCalc.State (State, pop, push)

	data Operator = Operator
		{arity_ :: Int
		,action_ :: ([Double] -> Double)
		,help_ :: String}

	binaryOperator :: [String] -> (Double -> Double -> Double) -> [(String, Operator)]
	binaryOperator names f = [(name, op) | name <- names]
		where
			op = Operator 2 action help
			action (x:y:[]) = y `f` x
			help = "[... x, y] -> x " ++ mainName ++ " y"
			mainName = head names

	binaryOperators :: [(String, Operator)]
	binaryOperators = concat $ map (\(n,f) -> binaryOperator n f)
		[(["+"], (+))
		,(["-"], (-))
		,(["/"], (/))
		,(["*"], (*))]


	operators :: Map.Map String Operator
	operators = Map.fromList binaryOperators

	isOperator :: String -> Bool
	isOperator name = Map.member name operators

	evalOperator :: State -> Operator -> State
	evalOperator state op = newState
		where
			newState = push poppedState newValue
			newValue = (action_ op) args
			(args, poppedState) = pop state (arity_ op)

	handleOperator :: State -> String -> Either String State
	handleOperator state opName = doOperator state foundOp
		where
			doOperator _ Nothing = Left $ "Operator " ++ opName ++ " not found."
			doOperator state (Just op) = Right $ evalOperator state op
			foundOp = Map.lookup opName operators
