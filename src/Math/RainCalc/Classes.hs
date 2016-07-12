module Math.RainCalc.Classes where

    class Documented d where
        getHelp :: d -> String
