module Math.RainCalc.RPNState where
import           Control.Monad.State.Strict as StateM

import           Data.List                (mapAccumL)
import qualified Data.Map                 as Map

data RPNState = RPNState
	{stack_ 	:: [Double]
	,registers_ :: Map.Map Char Double
	,modifier_ 	:: Maybe String
	,terminate_ :: Bool}

type Computation = StateM.State RPNState

formatStack :: [Double] -> String
formatStack [] = "(EMPTY)"
formatStack stack = header ++ contents
	where
		header = "STACK:\n(TOP)\n"
		contents = concat . snd $ mapAccumL accum 0 stack
		accum i x = (i + 1, formatLine i x)
		formatLine i x = show i ++ " : " ++ show x ++ "\n"

displayState :: RPNState -> String
displayState = formatStack . stack_

emptyState :: RPNState
emptyState = RPNState
	{stack_ = []
	,registers_ = Map.empty
	,modifier_ = Nothing
	,terminate_ = False}

terminate :: Computation ()
terminate = modify $ \state' -> state' {terminate_ = True}

stackSize :: RPNState -> Int
stackSize = length . stack_

smap :: ([Double] -> [Double]) -> RPNState -> RPNState
smap f state' = state' {stack_ = f $ stack_ state'}

rmap :: (Map.Map Char Double -> Map.Map Char Double) -> RPNState -> RPNState
rmap f state' = state' {registers_ = f $ registers_ state'}

push :: Double -> Computation ()
push = modify . push_

push_ :: Double -> RPNState -> RPNState
push_ num = smap (num:)

pop :: Int -> Computation [Double]
pop n = do
	stack <- gets stack_
	modify $ smap (drop n)
	return $ case stack of
		[] -> []
		_ -> take n stack

dup :: Computation ()
dup = modify dup_

dup_ :: RPNState -> RPNState
dup_ = smap newStk
	where
		newStk oldStk = case oldStk of
			[] -> oldStk
			stk@(x:_) -> x:stk

swap :: Computation ()
swap = modify swap_

swap_ :: RPNState -> RPNState
swap_ = smap newStk
	where
		newStk oldStk = case oldStk of
			(x:y:xs) -> y:x:xs
			_ -> oldStk

clear :: Computation ()
clear = modify clear_

clear_ :: RPNState -> RPNState
clear_ = smap (const [])
