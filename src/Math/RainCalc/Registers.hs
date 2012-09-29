module Math.RainCalc.Registers where
	import qualified Data.Map as Map
	import Data.Char (isAlpha, toLower)
	import Data.Maybe (fromMaybe)

	import qualified Math.RainCalc.State as State

	set :: State.State -> Char -> Double -> State.State
	set state r val = State.rmap (Map.insert r val) state

	get :: State.State -> Char -> Maybe Double
	get state r = Map.lookup r $ State.registers_ state

	isValid :: Char -> Bool
	isValid = isAlpha . toLower

	present :: State.State -> String
	present = concatMap presentPair . Map.toList . State.registers_
		where
			presentPair (k,v) = (show k) ++ ": " ++ (show v) ++ "\n"

