module Math.RainCalc.Engine where

import Control.Applicative (liftA2, liftA3)
import Control.Monad ((>=>))
import Control.Monad.State.Strict (runState)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Math.RainCalc.Actions (ActionEntry (..), actions)
import Math.RainCalc.CalcError (CalcError (MalformedInput))
import Math.RainCalc.Command qualified as Cmd
import Math.RainCalc.Computation qualified as C
import Math.RainCalc.Modifier qualified as Mod
import Math.RainCalc.Numbers qualified as N
import Math.RainCalc.Operator (evalOperator)
import Math.RainCalc.Operator qualified as Op
import Math.RainCalc.Output (Output (..), renderOutput)
import Math.RainCalc.RPNState qualified as S
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )

version :: String
version = "0.1.0.0"

defaultMain :: IO ()
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

mainLoop :: S.RPNState -> InputT IO ()
mainLoop state =
  getInputLine (prompt state) >>= maybe (return ()) (processLine state)

processLine :: S.RPNState -> String -> InputT IO ()
processLine state line = do
  let computation = processLine_ $ map toLower line
  let (out, state) = runState computation state
  outputStrLn out
  if S.terminate_ state
    then return ()
    else mainLoop state

processLine_ :: String -> C.Computation String
processLine_ line = do
  output <- traverse (processToken >=> renderOutput) (words line)
  modOutput <- Mod.cleanupModifiers >>= renderOutput
  return $ concat output ++ modOutput

processToken :: String -> C.Computation Output
processToken tok =
  Mod.isArmed >>= bool (processToken_ tok) (Mod.triggerModifier tok)

processToken_ :: String -> C.Computation Output
processToken_ tok = do
  do
    let entry = (Map.lookup tok actions)
    case entry of
      Just (ECommand cmd) -> Cmd.action_ cmd
      Just (EOperator op) -> pure Silent <* Op.evalOperator op
      Just (EModifier mod) -> pure Silent <* Mod.armModifier tok
      Nothing ->
        if N.isNumber tok
          then N.handleNumber tok *> pure Silent
          else return . ShowError $ MalformedInput tok
