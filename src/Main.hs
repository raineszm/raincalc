module Main where

import System.Console.Haskeline (runInputT, defaultSettings)
	
import qualified Math.RainCalc.Engine as Engine
import Math.RainCalc.State (emptyState)

main = runInputT defaultSettings $ Engine.mainLoop emptyState