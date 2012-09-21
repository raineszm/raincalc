module Math.RainCalc.Utils where

	expand :: [([String], a)] -> [(String, a)]
	expand = concat . map alias
		where
			alias (names, x) = [(name, x) | name <- names]
