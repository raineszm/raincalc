module Math.RainCalc.State where
	import qualified Data.Map as Map

	data State = State {
	 stack :: [Double]
	 , registers :: Map.Map Char Double
	}

	emptyState :: State
	emptyState = State { stack = [], registers = Map.empty }

	smap :: ([Double] -> [Double]) -> State -> State
	smap f (State stk reg) = (State (f stk) reg)

	push :: State -> Double -> State
	push state num = smap ((:) num) state

	-- Get the n topmost numbers
	pop :: State -> Int -> ([Double], State)
	pop (State stk reg) n = (take n stk, State (drop n stk) reg)
