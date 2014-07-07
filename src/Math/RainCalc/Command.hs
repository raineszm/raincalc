module Math.RainCalc.Command where
import Control.Applicative ((<$>))
import           Control.Monad.State.Lazy
import qualified Data.Map                 as Map

import           Math.RainCalc.Classes
import           Math.RainCalc.Registers  (present)
import           Math.RainCalc.RPNState   (Computation, clear, displayState,
                                           dup, pop, swap, terminate)
import           Math.RainCalc.Utils

data Command = Command
    {action_ :: Computation String
    ,help_   :: String}

instance Documented Command where
    getHelp = help_

printStack :: Command
printStack = Command
    {action_ = printStackAction
    ,help_ = "Prints the stack to the screen."}

printStackAction :: Computation String
printStackAction = gets displayState

popStack :: Command
popStack = Command
    {action_ = popStackAction
    ,help_ = "Pops and presents the top of the stack."}

popStackAction :: Computation String
popStackAction = do
        popped <- pop 1
        return $ case popped of
            [] -> "Empty Stack."
            [x] -> show x

silent :: Computation a -> Computation String
silent = (>>return "")

quitCommand :: Command
quitCommand = Command
    {action_ = silent terminate
    ,help_ = "Quits the program."}

dupCommand :: Command
dupCommand = Command
    {action_ = silent dup
    ,help_ = "[ ... x ] -> [ ... x, x ]"}

swapCommand :: Command
swapCommand = Command
    {action_ = silent swap
    ,help_ = "[ ... x, y ] -> [ ... y, x ]"}

clearCommand :: Command
clearCommand = Command
    {action_ = silent clear
    ,help_ = "[ ... ] -> []"}

printRegisters :: Command
printRegisters = Command
    {action_ = printRegisterAction
    ,help_ = "Prints the set registers to the screen."}

printRegisterAction :: Computation String
printRegisterAction = ("REGISTERS:\n"++) <$> gets present

commands :: Map.Map String Command
commands = Map.fromList $ expand
    [([".s"], printStack)
    ,(["."], popStack)
    ,([".q", ".quit", "q", "quit"], quitCommand)
    ,(["dup"], dupCommand)
    ,(["swap"], swapCommand)
    ,(["clear"], clearCommand)
    ,([".r", ".reg", "registers"], printRegisters)]

isCommand :: String -> Bool
isCommand tok = Map.member tok commands

handleCommand :: String -> Computation String
handleCommand tok = action_ foundCmd
    where
        (Just foundCmd) = Map.lookup tok commands
