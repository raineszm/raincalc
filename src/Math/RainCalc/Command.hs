{-# LANGUAGE LambdaCase #-}

module Math.RainCalc.Command (
    commands,
)
where

import Data.Functor ((<&>))
import Math.RainCalc.Computation (
    Computation,
    clear,
    dup,
    pop,
    swap,
    terminate,
 )
import Math.RainCalc.Output (Output (..), silent)

-- "Pops and presents the top of the stack."
popStack :: Computation Output
popStack =
    pop 1
        <&> \case
            [] -> Silent
            [x] -> Value x

commands :: [([String], Computation Output)]
commands =
    [ ([".s"], pure PrintStack) -- "Prints the stack to the screen."
    , (["."], popStack)
    , ([".q", ".quit", "q", "quit"], silent terminate) --  "Quits the program."
    , (["dup"], silent dup) -- , help_ = "[ ... x ] -> [ ... x, x ]"
    , (["swap"], silent swap) -- help_ = "[ ... x, y ] -> [ ... y, x ]"
    , (["clear"], silent clear) -- , help_ = "[ ... ] -> []"
    , ([".r", ".reg", "registers"], pure PrintRegisters) -- "Prints the set registers to the screen."
    ]
