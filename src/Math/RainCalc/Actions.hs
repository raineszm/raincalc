module Math.RainCalc.Actions where

import Data.Map.Strict qualified as Map
import Math.RainCalc.Command (Command, commands)
import Math.RainCalc.Modifier (Modifier (..), modifiers)
import Math.RainCalc.Operator (Operator (..), operators)

data ActionEntry
    = ECommand Command
    | EOperator Operator
    | EModifier Modifier

actions :: Map.Map String ActionEntry
actions =
    fmap ECommand commands
        <> fmap EOperator operators
        <> fmap EModifier modifiers
