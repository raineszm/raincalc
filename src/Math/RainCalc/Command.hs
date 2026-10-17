{-# LANGUAGE LambdaCase #-}

module Math.RainCalc.Command (
    Command (..),
    commands,
)
where

import Control.Applicative ((<**>))
import Control.Monad.State.Lazy
import Data.Functor (($>), (<&>))
import Data.Map qualified as Map
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

data Command = Command
    { action_ :: Computation Output
    , help_ :: String
    }

printStack :: Command
printStack =
    Command
        { action_ = pure PrintStack
        , help_ = "Prints the stack to the screen."
        }

popStack :: Command
popStack =
    Command
        { action_ = popStackAction
        , help_ = "Pops and presents the top of the stack."
        }

popStackAction :: Computation Output
popStackAction =
    pop 1
        <&> \case
            [] -> Silent
            [x] -> Value x

quitCommand :: Command
quitCommand = Command{action_ = terminate $> Silent, help_ = "Quits the program."}

dupCommand :: Command
dupCommand = Command{action_ = dup $> Silent, help_ = "[ ... x ] -> [ ... x, x ]"}

swapCommand :: Command
swapCommand =
    Command{action_ = swap $> Silent, help_ = "[ ... x, y ] -> [ ... y, x ]"}

clearCommand :: Command
clearCommand = Command{action_ = clear $> Silent, help_ = "[ ... ] -> []"}

printRegisters :: Command
printRegisters =
    Command
        { action_ = pure PrintRegisters
        , help_ = "Prints the set registers to the screen."
        }

commands :: Map.Map String Command
commands =
    Map.fromList $
        expand
            [ ([".s"], printStack)
            , (["."], popStack)
            , ([".q", ".quit", "q", "quit"], quitCommand)
            , (["dup"], dupCommand)
            , (["swap"], swapCommand)
            , (["clear"], clearCommand)
            , ([".r", ".reg", "registers"], printRegisters)
            ]
