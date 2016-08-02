module Math.RainCalc.Engine where
import           Control.Monad              (liftM2)
import           Control.Monad.State.Strict (runState)
import           Data.Char                  (toLower)
import           System.Console.Haskeline   (InputT, defaultSettings,
                                             getInputLine, outputStrLn,
                                             runInputT)

import qualified Math.RainCalc.Command      as Cmd
import qualified Math.RainCalc.Modifier     as Mod
import qualified Math.RainCalc.Numbers      as N
import qualified Math.RainCalc.Operator     as Op
import qualified Math.RainCalc.RPNState     as S

version :: String
version = "0.1.0.0"

defaultMain :: IO S.RPNState
defaultMain = runInputT defaultSettings $ do
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
mainLoop state = getInputLine (prompt state) >>=
    maybe (return state) (processLine state)

processLine :: S.RPNState -> String -> InputT IO S.RPNState
processLine state line = do
        newState <- doOutput (processLine_ $ map toLower line) state
        if S.terminate_ newState
            then return newState
            else mainLoop newState

processLine_ :: String -> S.Computation String
processLine_ line = liftM2 (++)
    (concat <$> mapM processToken (words line))
    Mod.cleanupModifiers


doOutput :: S.Computation String -> S.RPNState -> InputT IO S.RPNState
doOutput comp oldState = do
    let (out, newState) = runState comp oldState
    outputStrLn out
    return newState

emptyOutput :: S.Computation () -> S.Computation String
emptyOutput = (>> return "")

processToken :: String -> S.Computation String
processToken tok = do
    armed <- Mod.isArmed
    if armed then
        Mod.triggerModifier tok
        else processToken_ tok


processToken_ :: String -> S.Computation String
processToken_ tok
    | Mod.isModifier tok = emptyOutput $ Mod.armModifier tok
    | Op.isOperator tok = emptyOutput $ Op.handleOperator tok
    | Cmd.isCommand tok = Cmd.handleCommand tok
    | N.isNumber tok = emptyOutput $ N.handleNumber tok
    | otherwise = return $ "Unrecognized sequence \"" ++ tok ++  "\""
