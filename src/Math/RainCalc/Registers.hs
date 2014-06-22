module Math.RainCalc.Registers where
	import qualified Data.Map as Map
	import Data.Char (isAlpha, toLower)

	import qualified Math.RainCalc.RPNState as S

	set :: S.RPNState -> Char -> Double -> S.RPNState
	set state r val = S.rmap (Map.insert r val) state

	get :: S.RPNState -> Char -> Maybe Double
	get state r = Map.lookup r $ S.registers_ state

	isValid :: Char -> Bool
	isValid = isAlpha . toLower

	present :: S.RPNState -> String
	present = concatMap presentPair . Map.toList . S.registers_
		where
			presentPair (k,v) = show k ++ ": " ++ show v ++ "\n"

