module Math.RainCalc.RPNState where
	import qualified Data.Map as Map

	data RPNState = RPNState
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

	displayState :: RPNState -> String
	displayState = formatStack . stack_

	emptyState :: RPNState
	emptyState = RPNState
		{stack_ = []
		,registers_ = Map.empty
		,modifier_ = Nothing}

	stackSize :: RPNState -> Int
	stackSize = length . stack_

	smap :: ([Double] -> [Double]) -> RPNState -> RPNState
	smap f state = state {stack_ = f $ stack_ state}

	rmap :: (Map.Map Char Double -> Map.Map Char Double) -> RPNState -> RPNState
	rmap f state = state {registers_ = f $ registers_ state}

	push :: RPNState -> Double -> RPNState
	push state num = smap (num:) state

	-- Get the n topmost numbers
	pop :: RPNState -> Int -> ([Double], RPNState)
	pop state n = (popped,
	               state {stack_ = drop n stack})
		where
			stack = stack_ state
			popped = case stack of
				[] -> []
				_ -> take n stack

	dup :: RPNState -> RPNState
	dup state = state { stack_ = newStk }
		where
			newStk = case oldStk of
				[] -> oldStk
				stk@(x:_) -> x:stk
			oldStk = stack_ state


	swap :: RPNState -> RPNState
	swap state = state {stack_ = newStk}
		where
			newStk = case oldStk of
				(x:y:xs) -> y:x:xs
				_ -> oldStk
			oldStk = stack_ state

	clear :: RPNState -> RPNState
	clear state = state { stack_ = [] }

	sumStack :: RPNState -> RPNState
	sumStack state = state { stack_ = [sum stk]}
		where
			stk = stack_ state


