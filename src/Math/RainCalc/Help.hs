module Math.RainCalc.Help where

import Data.Map.Strict qualified as Map
import Math.RainCalc.Command (commands)
import Math.RainCalc.Modifier (modifiers)
import Math.RainCalc.Operator (operators)

showActions :: String
showActions = unlines outLines
  where
    outLines =
        header
            : map
                doWrap
                [ Map.keys operators
                , Map.keys commands
                , Map.keys modifiers
                ]
    header = "Available Commands: "
    doWrap = wrapLine 60 . unwords

wrapLine :: Int -> String -> String
wrapLine n text = doWrap n ("", words text)

doWrap :: Int -> (String, [String]) -> String
doWrap _ (acc, []) = acc
doWrap n (acc, text) = doWrap n (acc ++ ('\n' : unwords nextLine), rest)
  where
    (nextLine, rest) = wrapLine' n (0, [], text)

wrapLine' :: Int -> (Int, [String], [String]) -> ([String], [String])
wrapLine' _ (_, y, []) = (y, [])
wrapLine' n (i, y, rest@(x : xs))
    | newlength < n = wrapLine' n (newlength, y ++ [x], xs)
    | otherwise = (y, rest)
  where
    newlength = i + length x
