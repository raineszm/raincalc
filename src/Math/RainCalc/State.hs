module Math.RainCalc.State where
	import qualified Data.Map as Map

	data State = State
		{stack_ :: [Double]
		,registers_ :: Map.Map Char Double
		,modifier_ :: Maybe String }

	formatStack :: [Double] -> String
	formatStack stack = header ++ contents
		where
			header = "STACK:\n(TOP)\n"
			contents = concat [formatLine i entry| (i, entry) <- zip [0..size] stack]
			formatLine i entry = show i ++ " : " ++ show entry ++ "\n"
			size = length stack

	instance Show State where
		show = formatStack . stack_

	emptyState :: State
	emptyState = State 
		{stack_ = []
		,registers_ = Map.empty
		,modifier_ = Nothing}

	stackSize :: State -> Int
	stackSize = length . stack_

	smap :: ([Double] -> [Double]) -> State -> State
	smap f state = state {stack_ = f $ stack_ state}

	push :: State -> Double -> State
	push state num = smap (num:) state

	-- Get the n topmost numbers
	pop :: State -> Int -> ([Double], State)
	pop state n = (popped,
	               state {stack_ = drop n stack})
		where
			stack = stack_ state
			popped = case stack of
				[] -> []
				_ -> take n stack

	dup :: State -> State
	dup state = state { stack_ = newStk }
		where
			newStk = case oldStk of
				[] -> oldStk
				stk@(x:xs) -> x:stk
			oldStk = stack_ state


	swap :: State -> State
	swap state = state {stack_ = newStk}
		where
			newStk = case oldStk of 
				(x:y:xs) -> y:x:xs
				_ -> oldStk
			oldStk = stack_ state
