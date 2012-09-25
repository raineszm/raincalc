module Math.RainCalc.Operator where
	import qualified Data.Map as Map
	import Data.List (intercalate)

	import Math.RainCalc.State (State, pop, push)
	import Math.RainCalc.Utils (expandWith)

	data Operator = Operator
		{arity_ :: Int
		,action_ :: ([Double] -> Double)
		,help_ :: String}

	binaryOperator :: [String] -> (Double -> Double -> Double) -> Operator
	binaryOperator names f = Operator 
		{arity_ = 2
		,action_ = action
		,help_ = help}
		where
			action (x:y:[]) = y `f` x
			help = simpleHelp mainName 2 
			mainName = head names

	binaryOperators :: [(String, Operator)]
	binaryOperators = expandWith binaryOperator
		[(["+"], (+))
		,(["-"], (-))
		,(["/"], (/))
		,(["*"], (*))]

	simpleHelp :: String -> Int -> String
	simpleHelp name 0 = "[... ] -> [... " ++ name ++ "]"
	simpleHelp name 1 = "[... x] -> [... " ++ name ++ "(x)]"
	simpleHelp name 2 = "[... x, y] -> [... x " ++ name ++ " y]"
	simpleHelp name arity = "[... " ++ vars ++ "] -> [... " ++ name ++ "(" ++ vars ++")]"
		where
			vars = intercalate ", " $ map (('x':).show) [1..arity]

	buildOperator :: Int -> [String] -> ([Double] -> Double) -> Operator
	buildOperator arity names f = Operator 
		{arity_ = arity
		,action_ = f
		,help_ = simpleHelp (head names) arity}

	unaryOperators :: [(String, Operator)]
	unaryOperators = expandWith (buildOperator 1)
		[(["sin"], sin . head)
		,(["cos"], cos . head)
		,(["tan"], tan . head)]

	constants :: [(String, Operator)]
	constants = expandWith (buildOperator 0)
		[(["e"], \_ -> exp 1),
		(["pi"], \_ -> pi)]

	operators :: Map.Map String Operator
	operators = Map.fromList $ concat
		[constants
		,unaryOperators
		,binaryOperators]

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
