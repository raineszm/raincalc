module Main where

import System.Console.Haskeline (runInputT, defaultSettings)

import qualified Math.RainCalc.Engine as Engine
import Math.RainCalc.RPNState (RPNState, emptyState)

main :: IO RPNState
main = runInputT defaultSettings $ Engine.mainLoop emptyState