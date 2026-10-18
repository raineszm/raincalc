{-# LANGUAGE LambdaCase #-}

module Math.RainCalc.Modifier (
    takeModifier,
    cleanupModifiers,
    armModifier,
    modifiers,
)
where

import Control.Monad.State.Lazy (gets, modify)
import Data.List (stripPrefix)
import Data.Maybe (
    fromJust,
    fromMaybe,
    isNothing,
 )
import Math.RainCalc.Action (Modifier (..), Session, SessionState (..), zoomRPN)
import Math.RainCalc.CalcError (CalcError (..))
import Math.RainCalc.Computation (Computation, pop, push)
import Math.RainCalc.Output (Output (..), silent)
import Math.RainCalc.Registers qualified as Reg
import Numeric (readHex)
import Prelude hiding (mod)

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
            [x] -> silent $ Reg.setR c x
            _ -> pure . ShowError $ EmptyStack
    | otherwise = pure . ShowError $ InvalidRegister [c]
toRegisterAction s = pure . ShowError $ InvalidRegister s

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

armModifier :: Modifier -> Session ()
armModifier mod = modify $ \state -> state{activeModifier = Just mod}

disarmModifier :: Session ()
disarmModifier = modify $ \state -> state{activeModifier = Nothing}

takeModifier :: Session (Maybe Modifier)
takeModifier = gets activeModifier <* disarmModifier

cleanupModifiers :: Session Output
cleanupModifiers = do
    mod <- takeModifier
    case mod of
        Nothing -> pure Silent
        Just mod' -> zoomRPN $ runModSolo mod'
