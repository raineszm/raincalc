{-# LANGUAGE LambdaCase #-}

module Math.RainCalc.Command (
    commands,
)
where

import Control.Applicative ((<**>))
import Control.Monad.State.Lazy
import Data.Functor (($>), (<&>))
import Data.Map qualified as Map
import Math.RainCalc.Action (Command (..))
import Math.RainCalc.Computation (
    Computation,
    clear,
    dup,
    pop,
    swap,
    terminate,
 )
import Math.RainCalc.Output (Output (..), renderOutput)
import Math.RainCalc.RPNState (displayState)
import Math.RainCalc.Registers (present)
import Math.RainCalc.Utils

printStack :: Command
printStack =
    Command
        { runCmd = pure PrintStack
        -- , help_ = "Prints the stack to the screen."
        }

popStack :: Command
popStack =
    Command
        { runCmd = popStackAction
        -- , help_ = "Pops and presents the top of the stack."
        }

popStackAction :: Computation Output
popStackAction =
    pop 1
        <&> \case
            [] -> Silent
            [x] -> Value x

quitCommand :: Command
quitCommand =
    Command
        { runCmd = terminate $> Silent
        -- help_ = "Quits the program."
        }

dupCommand :: Command
dupCommand =
    Command
        { runCmd = dup $> Silent
        -- , help_ = "[ ... x ] -> [ ... x, x ]"
        }

swapCommand :: Command
swapCommand =
    Command
        { runCmd = swap $> Silent
        -- help_ = "[ ... x, y ] -> [ ... y, x ]"
        }

clearCommand :: Command
clearCommand =
    Command
        { runCmd = clear $> Silent
        -- , help_ = "[ ... ] -> []"
        }

printRegisters :: Command
printRegisters =
    Command
        { runCmd = pure PrintRegisters
        -- , help_ = "Prints the set registers to the screen."
        }

commands :: [([String], Command)]
commands =
    [ ([".s"], printStack)
    , (["."], popStack)
    , ([".q", ".quit", "q", "quit"], quitCommand)
    , (["dup"], dupCommand)
    , (["swap"], swapCommand)
    , (["clear"], clearCommand)
    , ([".r", ".reg", "registers"], printRegisters)
    ]
