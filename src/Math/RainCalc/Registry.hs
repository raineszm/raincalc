module Math.RainCalc.Registry where

import Data.Map.Strict qualified as Map
import Math.RainCalc.Action (ActionEntry (..))
import Math.RainCalc.Command (commands)
import Math.RainCalc.Modifier (modifiers)
import Math.RainCalc.Operator (operators)

actions :: Map.Map String ActionEntry
actions =
    fmap ECommand (Map.fromList $ expand commands)
        <> fmap EOperator (Map.fromList $ expand operators)
        <> fmap EModifier (Map.fromList $ expand modifiers)

expand :: [([String], a)] -> [(String, a)]
expand = foldMap alias
  where
    alias (names, x) = map (,x) names
