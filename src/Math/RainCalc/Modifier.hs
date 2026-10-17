{-# LANGUAGE LambdaCase #-}

module Math.RainCalc.Modifier
  ( triggerModifier,
    cleanupModifiers,
    armModifier,
    isModifier,
    isArmed,
  )
where

import Control.Monad ((<=<), (>=>))
import Control.Monad.State.Lazy (gets, modify)
import Data.List (stripPrefix)
import Data.Map qualified as Map
import Data.Maybe
  ( fromJust,
    fromMaybe,
    isJust,
    isNothing,
  )
import Math.RainCalc.CalcError (CalcError (..))
import Math.RainCalc.Classes
import Math.RainCalc.Command (commands, isCommand)
import Math.RainCalc.Computation (Computation, pop, push)
import Math.RainCalc.Operator (isOperator, operators)
import Math.RainCalc.Output (Output (..), renderOutput)
import Math.RainCalc.RPNState (RPNState, modifier_)
import Math.RainCalc.Registers qualified as Reg
import Math.RainCalc.Utils
import Numeric (readHex)

data Modifier = Modifier
  { action_ :: String -> Computation Output,
    solo_ :: Computation Output,
    help_ :: String
  }

instance Documented Modifier where
  getHelp = help_

help :: Modifier
help =
  Modifier
    { action_ = helpAction,
      solo_ = soloHelpAction,
      help_ = "Displays help information"
    }

soloHelpAction :: Computation Output
soloHelpAction = (return . Message . unlines) outLines
  where
    outLines =
      header
        : map doWrap [Map.keys operators, Map.keys commands, Map.keys modifiers]
    header = "Available Commands: "
    doWrap = wrapLine 60 . unwords

helpAction :: String -> Computation Output
helpAction cmd
  | isModifier cmd = Message <$> helpFrom modifiers
  | isCommand cmd = Message <$> helpFrom commands
  | isOperator cmd = Message <$> helpFrom operators
  | otherwise = return . ShowError $ MalformedInput cmd
  where
    helpFrom :: (Documented v) => Map.Map String v -> Computation String
    helpFrom = return . formatHelp cmd

formatHelp :: (Documented v) => String -> Map.Map String v -> String
formatHelp key objs = "Help for " ++ key ++ ":\n" ++ getHelp obj
  where
    obj = fromJust $ Map.lookup key objs

hex :: Modifier
hex =
  Modifier
    { action_ = hexAction,
      solo_ = return . ShowError $ MalformedInput "x:",
      help_ = "Read next input as a hex sequence."
    }

hexAction :: String -> Computation Output
hexAction tok =
  if null rest
    then push num >> return Silent
    else return $ Message $ "Invalid hex sequence: " ++ tok
  where
    ((num, rest) : _) = readHex digits
    digits = fromMaybe tok $ stripPrefix "0x" tok

toRegister :: Modifier
toRegister =
  Modifier
    { action_ = toRegisterAction,
      solo_ = soloRegisterAction,
      help_ = "Pops the top of the stack to the specified register."
    }

soloRegisterAction :: Computation Output
soloRegisterAction = return $ Message "No register supplied."

toRegisterAction :: String -> Computation Output
toRegisterAction [c]
  | Reg.isValid c =
      pop 1 >>= \case
        [x] -> Reg.setR c x >> return Silent
        _ -> return . ShowError $ EmptyStack
  | otherwise = return . ShowError $ InvalidRegister [c]
toRegisterAction s = return . ShowError $ InvalidRegister s

fromRegister :: Modifier
fromRegister =
  Modifier
    { action_ = fromRegisterAction,
      solo_ = soloRegisterAction,
      help_ = "Pushes the value of the specified register to the stack."
    }

fromRegisterAction :: String -> Computation Output
fromRegisterAction [c]
  | Reg.isValid c = do
      fetched <- Reg.getR c
      if isNothing fetched
        then return $ Message ("Tried to fetch unset register " ++ [c])
        else push (fromJust fetched) >> return Silent
  | otherwise = return . ShowError $ InvalidRegister [c]
fromRegisterAction s = return . ShowError $ InvalidRegister s

isModifier :: String -> Bool
isModifier = (`Map.member` modifiers)

modifiers :: Map.Map String Modifier
modifiers =
  Map.fromList $
    expand
      [ (["?", ".?", ".h", ".help"], help),
        (["x:"], hex),
        (["->"], toRegister),
        (["<-"], fromRegister)
      ]

armModifier :: String -> Computation ()
armModifier modName = modify $ \state -> state {modifier_ = Just modName}

disarmModifier :: Computation ()
disarmModifier = modify $ \state -> state {modifier_ = Nothing}

isArmed_ :: RPNState -> Bool
isArmed_ = isJust . modifier_

isArmed :: Computation Bool
isArmed = gets isArmed_

activeModifier :: RPNState -> Maybe Modifier
activeModifier = modifier_ >=> (`Map.lookup` modifiers)

doAction :: String -> Maybe Modifier -> Computation String
doAction tok =
  maybe (return "Somehow a modifier became undefined") (renderOutput <=< (`action_` tok))

triggerModifier :: String -> Computation String
triggerModifier tok = gets activeModifier <* disarmModifier >>= doAction tok

cleanupModifiers :: Computation String
cleanupModifiers =
  gets activeModifier <* disarmModifier >>= maybe (return "") (renderOutput <=< solo_)
