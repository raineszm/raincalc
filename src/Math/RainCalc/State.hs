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

	smap :: ([Double] -> [Double]) -> State -> State
	smap f state = state {stack_ = f $ stack_ state}

	push :: State -> Double -> State
	push state num = smap (num:) state

	-- Get the n topmost numbers
	pop :: State -> Int -> ([Double], State)
	pop state n = (take n stack,
	               state {stack_ = drop n stack})
		where
			stack = stack_ state
