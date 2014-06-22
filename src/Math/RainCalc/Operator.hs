module Math.RainCalc.Operator where
	import qualified Data.Map as Map
	import Data.List (intercalate)

	import Math.RainCalc.RPNState (RPNState, pop, push)
	import Math.RainCalc.Utils (expandWith)
	import Math.RainCalc.Classes

	data Operator = Operator
		{arity_ :: Int
		,action_ :: [Double] -> Double
		,help_ :: String}

	instance Documented Operator where
		getHelp = help_

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
		[(["sin", "s"], sin . head)
		,(["cos", "c"], cos . head)
		,(["tan", "t"], tan . head)
		,(["sinh", "sh"], sinh . head)
		,(["cosh", "ch"], cosh . head)
		,(["tanh", "th"], tanh . head)
		,(["atan"], atan . head)
		,(["acos"], acos . head)
		,(["asin"], asin . head)
		,(["exp"], exp . head)
		,(["atanh"], atanh . head)
		,(["acosh"], acosh . head)
		,(["asinh"], asinh . head)
		,(["exp"], exp . head)
		,(["ln", "log"], log . head)
		,(["deg", "todegrees"], toDegrees . head)
		,(["rad", "toradians"], toRadians . head)
		,(["factorial", "fact", "!"], factorial . head)
		,(["inv", "1/"], (1/) . head)
		,(["neg"], negate . head)
		,(["signum", "sgn", "sign"], signum . head)
		,(["abs", "||"], abs . head)
		,(["sqrt", "v"], sqrt . head)]

	toDegrees :: Double -> Double
	toDegrees = (180*) . (/pi)

	toRadians :: Double -> Double
	toRadians = (pi*) . (/180)

	factorial :: Double -> Double
	factorial 0 = 1
	factorial n
		| n < 0 = 0/0
		| otherwise = fromIntegral $ product [1..(round n :: Integer)]


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

	evalOperator :: RPNState -> Operator -> RPNState
	evalOperator state op = if numArgs < arity then state else newState
		where
			newState = push poppedState newValue
			newValue = action_ op args
			numArgs = length args
			(args, poppedState) = pop state arity
			arity = arity_ op

	handleOperator :: RPNState -> String -> RPNState
	handleOperator state opName = doOperator state foundOp
		where
			doOperator _ Nothing = error ("Operator " ++ opName ++ " not found.")
			doOperator state' (Just op) = evalOperator state' op
			foundOp = Map.lookup opName operators
