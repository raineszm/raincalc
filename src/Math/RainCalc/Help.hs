module Math.RainCalc.Help where

import Data.List (intercalate)
import Data.Map.Strict qualified as Map

showActions :: [[String]] -> String
showActions sections = unlines outLines
  where
    outLines =
        header
            : map
                doWrap
                sections
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

simpleHelp :: String -> Int -> String
simpleHelp name (-1) = "[... ] -> [" ++ name ++ "(...)]"
simpleHelp name 0 = "[... ] -> [... " ++ name ++ "]"
simpleHelp name 1 = "[... x] -> [... " ++ name ++ "(x)]"
simpleHelp name 2 = "[... x, y] -> [... x " ++ name ++ " y]"
simpleHelp name arity =
    "[... " ++ vars ++ "] -> [... " ++ name ++ "(" ++ vars ++ ")]"
  where
    vars = intercalate ", " $ map (('x' :) . show) [1 .. arity]
