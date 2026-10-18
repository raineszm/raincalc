module Math.RainCalc.Engine where

import Control.Monad ((>=>))
import Control.Monad.State.Strict (runState)
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Math.RainCalc.Action (ActionEntry (..), Modifier (runMod), Session, SessionState (..), sessionState, zoomRPN)
import Math.RainCalc.CalcError (CalcError (MalformedInput))
import Math.RainCalc.Modifier qualified as Mod
import Math.RainCalc.Numbers qualified as N
import Math.RainCalc.Operator qualified as Op
import Math.RainCalc.Output (Output (..), renderOutput, silent)
import Math.RainCalc.RPNState qualified as S
import Math.RainCalc.Registry (actions)
import System.Console.Haskeline (
    InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
 )
import Prelude hiding (mod)

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
    let session = processLine_ $ map toLower line
    let (out, SessionState{rpn = state'}) = runState session (sessionState state)
    outputStrLn out
    if S.terminate_ state'
        then return ()
        else mainLoop state'

processLine_ :: String -> Session String
processLine_ line = do
    output <- traverse (processToken >=> render) (words line)
    modOutput <- Mod.cleanupModifiers >>= render
    return $ concat output ++ modOutput
  where
    render = zoomRPN . renderOutput

processToken :: String -> Session Output
processToken tok = do
    mod <- Mod.takeModifier
    case mod of
        Nothing -> processToken_ tok
        Just mod' -> zoomRPN $ runMod mod' tok

processToken_ :: String -> Session Output
processToken_ tok = do
    do
        let entry = Map.lookup tok actions
        case entry of
            Just (ECommand cmd) -> zoomRPN cmd
            Just (EOperator op) -> silent . zoomRPN $ Op.evalOperator op
            Just (EModifier mod) -> silent $ Mod.armModifier mod
            Nothing ->
                if N.isNumber tok
                    then silent . zoomRPN $ N.handleNumber tok
                    else return . ShowError $ MalformedInput tok
