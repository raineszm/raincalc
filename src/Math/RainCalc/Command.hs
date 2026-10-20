{-# LANGUAGE LambdaCase #-}

module Math.RainCalc.Command (
    commands,
)
where

import Data.Functor ((<&>))
import Math.RainCalc.Action (Action (..))
import Math.RainCalc.Computation (
    Computation,
    clear,
    dup,
    pop,
    swap,
    terminate,
 )
import Math.RainCalc.Output (Output (..), silent)

commands :: [Action (Computation Output)]
commands =
    [ Action
        { names = [".s"]
        , actionSpec = pure PrintStack
        , desc = "Prints the stack to the screen."
        }
    , Action
        { names = ["."]
        , actionSpec =
            pop 1
                <&> \case
                    [] -> Silent
                    [x] -> Value x
                    _ -> error "Pop 1 returned more than one value"
        , desc = "Pops and presents the top of the stack."
        }
    , Action
        { names = [".q", ".quit", "q", "quit"]
        , actionSpec = silent terminate
        , desc = "Quits the program."
        }
    , Action
        { names = ["dup"]
        , actionSpec = silent dup
        , desc = "[ ... x ] -> [ ... x, x ]"
        }
    , Action
        { names = ["swap"]
        , actionSpec = silent swap
        , desc = "[ ... x, y ] -> [ ... y, x ]"
        }
    , Action
        { names = ["clear"]
        , actionSpec = silent clear
        , desc = "[ ... ] -> []"
        }
    , Action
        { names = [".r", ".reg", "registers"]
        , actionSpec = pure PrintRegisters
        , desc = "Prints the set registers to the screen."
        }
    ]
