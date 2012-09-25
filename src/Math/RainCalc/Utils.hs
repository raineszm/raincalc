module Math.RainCalc.Utils where
	import Data.List (concatMap)

	expandWith :: ([String] -> a -> b) -> [([String], a)] -> [(String, b)]
	expandWith f = concatMap alias
		where
			alias (names, x) = map (,f names x) names

	expand :: [([String], a)] -> [(String, a)]
	expand = expandWith (\_ -> id) 
