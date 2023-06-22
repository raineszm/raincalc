{-# LANGUAGE LambdaCase #-}

module Math.RainCalc.RPNState
  ( RPNState (..),
    emptyState,
    smap,
    rmap,
    formatStack,
    displayState,
    swap_,
    stackSize,
    push_,
    dup_,
    clear_,
  )
where

import Data.List (mapAccumL)
import qualified Data.Map as Map

data RPNState = RPNState
  { stack_ :: [Double],
    registers_ :: Map.Map Char Double,
    modifier_ :: Maybe String,
    terminate_ :: Bool
  }

formatStack :: [Double] -> String
formatStack [] = "(EMPTY)"
formatStack stack = header ++ contents
  where
    header = "STACK:\n(TOP)\n"
    contents = concat . snd $ mapAccumL accum (0 :: Int) stack
    accum i x = (i + 1, formatLine i x)
    formatLine i x = show i ++ " : " ++ show x ++ "\n"

displayState :: RPNState -> String
displayState = formatStack . stack_

emptyState :: RPNState
emptyState =
  RPNState
    { stack_ = [],
      registers_ = Map.empty,
      modifier_ = Nothing,
      terminate_ = False
    }

stackSize :: RPNState -> Int
stackSize = length . stack_

smap :: ([Double] -> [Double]) -> RPNState -> RPNState
smap f state' = state' {stack_ = f $ stack_ state'}

rmap :: (Map.Map Char Double -> Map.Map Char Double) -> RPNState -> RPNState
rmap f state' = state' {registers_ = f $ registers_ state'}

push_ :: Double -> RPNState -> RPNState
push_ num = smap (num :)

dup_ :: RPNState -> RPNState
dup_ =
  smap $ \case
    [] -> []
    stk@(x : _) -> x : stk

swap_ :: RPNState -> RPNState
swap_ =
  smap $ \case
    (x : y : xs) -> y : x : xs
    xs -> xs

clear_ :: RPNState -> RPNState
clear_ = smap (const [])
