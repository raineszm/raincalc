{-# LANGUAGE LambdaCase #-}

module Math.RainCalc.Modifier (
    triggerModifier,
    cleanupModifiers,
    armModifier,
    isArmed,
    modifiers,
)
where

import Control.Monad ((<=<), (>=>))
import Control.Monad.State.Lazy (gets, modify)
import Data.List (stripPrefix)
import Data.Map qualified as Map
import Data.Maybe (
    fromJust,
    fromMaybe,
    isJust,
    isNothing,
 )
import GHC.OldList (find)
import Math.RainCalc.Action (Modifier (..))
import Math.RainCalc.CalcError (CalcError (..))
import Math.RainCalc.Computation (Computation, pop, push)
import Math.RainCalc.Operator (operators)
import Math.RainCalc.Output (Output (..), renderOutput)
import Math.RainCalc.RPNState (RPNState, modifier_)
import Math.RainCalc.Registers qualified as Reg
import Math.RainCalc.Utils
import Numeric (readHex)

hex :: Modifier
hex =
    Modifier
        { runMod = hexAction
        , runModSolo = return . ShowError $ MalformedInput "x:"
        -- , help_ = "Read next input as a hex sequence."
        }

hexAction :: String -> Computation Output
hexAction tok =
    if null rest
        then push num >> return Silent
        else return . ShowError . MalformedInput $ "Invalid hex sequence: " ++ tok
  where
    ((num, rest) : _) = readHex digits
    digits = fromMaybe tok $ stripPrefix "0x" tok

toRegister :: Modifier
toRegister =
    Modifier
        { runMod = toRegisterAction
        , runModSolo = soloRegisterAction
        -- , help_ = "Pops the top of the stack to the specified register."
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
        { runMod = fromRegisterAction
        , runModSolo = soloRegisterAction
        -- , help_ = "Pushes the value of the specified register to the stack."
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

modifiers :: [([String], Modifier)]
modifiers =
    [ -- (["?", ".?", ".h", ".help"], help),
      (["x:"], hex)
    , (["->"], toRegister)
    , (["<-"], fromRegister)
    ]

armModifier :: String -> Computation ()
armModifier modName = modify $ \state -> state{modifier_ = Just modName}

disarmModifier :: Computation ()
disarmModifier = modify $ \state -> state{modifier_ = Nothing}

isArmed :: Computation Bool
isArmed = gets $ isJust . modifier_

activeModifier :: RPNState -> Maybe Modifier
activeModifier = modifier_ >=> getMod
  where
    getMod name = fmap snd $ find (matches name) modifiers
    matches name (a, _) = elem name a

takeModifier :: Computation (Maybe Modifier)
takeModifier = gets activeModifier <* disarmModifier

doAction :: String -> Maybe Modifier -> Computation Output
doAction tok =
    maybe (return . Message $ "Somehow a modifier became undefined") (`runMod` tok)

triggerModifier :: String -> Computation Output
triggerModifier tok = do
    mod <- takeModifier
    doAction tok mod

cleanupModifiers :: Computation Output
cleanupModifiers = do
    mod <- takeModifier
    case mod of
        Nothing -> return Silent
        Just mod' -> runModSolo mod'
