module Math.RainCalc.Engine where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.State.Strict (runState)
import Data.Bool (bool)
import Data.Char (toLower)
import qualified Math.RainCalc.Command as Cmd
import qualified Math.RainCalc.Computation as C
import qualified Math.RainCalc.Modifier as Mod
import qualified Math.RainCalc.Numbers as N
import qualified Math.RainCalc.Operator as Op
import qualified Math.RainCalc.RPNState as S
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )

version :: String
version = "0.1.0.0"

defaultMain :: IO S.RPNState
defaultMain =
  runInputT defaultSettings $ do
    outputStrLn "--------------------------------"
    outputStrLn $ "raincalc v" ++ version
    outputStrLn "<math and stuff>"
    outputStrLn "--------------------------------"
    outputStrLn ""
    mainLoop S.emptyState

prompt :: S.RPNState -> String
prompt state = "raincalc[s:" ++ size ++ "]> "
  where
    size = show $ S.stackSize state

mainLoop :: S.RPNState -> InputT IO S.RPNState
mainLoop state =
  getInputLine (prompt state) >>= maybe (return state) (processLine state)

processLine :: S.RPNState -> String -> InputT IO S.RPNState
processLine state line =
  doOutput (processLine_ $ map toLower line) state
    >>= liftA3 bool mainLoop return S.terminate_

processLine_ :: String -> C.Computation String
processLine_ line =
  liftA2
    (++)
    (concat <$> traverse processToken (words line))
    Mod.cleanupModifiers

doOutput :: C.Computation String -> S.RPNState -> InputT IO S.RPNState
doOutput comp oldState =
  let (out, newState) = runState comp oldState
   in newState <$ outputStrLn out

processToken :: String -> C.Computation String
processToken tok =
  Mod.isArmed >>= bool (processToken_ tok) (Mod.triggerModifier tok)

processToken_ :: String -> C.Computation String
processToken_ tok
  | Mod.isModifier tok = "" <$ Mod.armModifier tok
  | Op.isOperator tok = "" <$ Op.handleOperator tok
  | Cmd.isCommand tok = Cmd.handleCommand tok
  | N.isNumber tok = "" <$ N.handleNumber tok
  | otherwise = return $ "Unrecognized sequence \"" ++ tok ++ "\""
