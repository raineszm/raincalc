module Math.RainCalc.Utils
	(expandWith
	,expand
	,wrapLine)
	where

	expandWith :: ([String] -> a -> b) -> [([String], a)] -> [(String, b)]
	expandWith f = concatMap alias
		where
			alias (names, x) = map (,f names x) names

	expand :: [([String], a)] -> [(String, a)]
	expand = expandWith (\_ -> id)

	wrapLine :: Int -> String -> String
	wrapLine n text = doWrap n ("", words text)

	doWrap :: Int -> (String, [String]) -> String
	doWrap _ (acc, []) = acc
	doWrap n (acc, text) = doWrap n (acc ++ ('\n':unwords nextLine), rest)
		where
			(nextLine, rest) = wrapLine' n (0, [], text)

	wrapLine' :: Int -> (Int, [String], [String]) -> ([String], [String])
	wrapLine' _ (_, y, []) = (y, [])
	wrapLine' n (i, y, rest@(x:xs))
		| newlength < n = wrapLine' n (newlength, y ++ [x], xs)
		| otherwise = (y, rest)
		where
			newlength = i + length x
