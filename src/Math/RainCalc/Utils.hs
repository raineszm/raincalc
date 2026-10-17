{-# LANGUAGE TupleSections #-}

module Math.RainCalc.Utils
  ( expandWith,
    expand,
  )
where

expandWith :: ([String] -> a -> b) -> [([String], a)] -> [(String, b)]
expandWith f = foldMap alias
  where
    alias (names, x) = map (,f names x) names

expand :: [([String], a)] -> [(String, a)]
expand = expandWith $ const id
