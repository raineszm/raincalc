{-# LANGUAGE LambdaCase #-}

module Math.RainCalc.Modifier (
    takeModifier,
    cleanupModifiers,
    armModifier,
    modifiers,
    helpSpec,
)
where

import Control.Monad.State.Class (gets, modify)
import Data.List (stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Maybe (
    fromMaybe,
 )
import Math.RainCalc.Action (Action (..), ActionEntry, ModifierSpec (..), Session, SessionState (..), actionDesc, zoomRPN)
import Math.RainCalc.CalcError (CalcError (..))
import Math.RainCalc.Computation (Computation, pop, push)
import Math.RainCalc.Help (showActions)
import Math.RainCalc.Output (Output (..), silent)
import Math.RainCalc.Registers qualified as Reg
import Numeric (readHex)
import Prelude hiding (mod)

hex :: ModifierSpec
hex =
    ModifierSpec
        { runMod = hexAction
        , runModSolo = return . ShowError $ MalformedInput "x:"
        }

hexAction :: String -> Computation Output
hexAction tok =
    if null rest
        then push num >> return Silent
        else return . ShowError . MalformedInput $ "Invalid hex sequence: " ++ tok
  where
    ((num, rest) : _) = readHex digits
    digits = fromMaybe tok $ stripPrefix "0x" tok

toRegister :: ModifierSpec
toRegister =
    ModifierSpec
        { runMod = toRegisterAction
        , runModSolo = soloRegisterAction
        }

registerAction :: (Char -> Computation Output) -> String -> Computation Output
registerAction go [c]
    | Reg.isValid c = go c
    | otherwise = pure . ShowError $ InvalidRegister [c]
registerAction _ s = pure . ShowError $ InvalidRegister s

soloRegisterAction :: Computation Output
soloRegisterAction = return $ Message "No register supplied."

toRegisterAction :: String -> Computation Output
toRegisterAction = registerAction $ \c -> do
    pop 1 >>= \case
        [x] -> silent $ Reg.setR c x
        _ -> pure . ShowError $ EmptyStack

fromRegister :: ModifierSpec
fromRegister =
    ModifierSpec
        { runMod = fromRegisterAction
        , runModSolo = soloRegisterAction
        }

fromRegisterAction :: String -> Computation Output
fromRegisterAction = registerAction $ \c -> do
    fetched <- Reg.getR c
    maybe
        (pure $ Message ("Tried to fetch unset register " ++ [c]))
        (silent . push)
        fetched

registersSpec :: ModifierSpec
registersSpec =
    ModifierSpec
        { runMod = runMod
        , runModSolo = pure PrintRegisters
        }
  where
    runMod = registerAction $ \c -> do
        fetched <- Reg.getR c
        pure . Message $
            c
                : ' '
                : maybe " unset" show fetched

modifiers :: [Action ModifierSpec]
modifiers =
    [ Action{names = ["x:"], actionSpec = hex, desc = "Read next input as a hex sequence."}
    , Action
        { names = ["->"]
        , actionSpec = toRegister
        , desc = "Pops the top of the stack to the specified register."
        }
    , Action
        { names = ["<-"]
        , actionSpec = fromRegister
        , desc = "Pushes the value of the specified register to the stack."
        }
    , Action
        { names = [".r", ".reg", "registers"]
        , actionSpec = registersSpec
        , desc = "Prints the set registers to the screen."
        }
    ]

helpSpec :: Map.Map String ActionEntry -> ModifierSpec
helpSpec actions =
    ModifierSpec
        { runMod = helpAction
        , runModSolo = soloHelpAction
        }
  where
    helpAction :: String -> Computation Output
    helpAction tok = do
        case Map.lookup tok actions of
            Just entry -> pure . Message $ actionDesc entry
            Nothing -> pure . Message $ "No such entry"
    soloHelpAction :: Computation Output
    soloHelpAction = do
        pure . Message $
            showActions
                [ Map.keys actions
                ]

armModifier :: ModifierSpec -> Session ()
armModifier mod = modify $ \state -> state{activeModifier = Just mod}

disarmModifier :: Session ()
disarmModifier = modify $ \state -> state{activeModifier = Nothing}

takeModifier :: Session (Maybe ModifierSpec)
takeModifier = gets activeModifier <* disarmModifier

cleanupModifiers :: Session Output
cleanupModifiers = do
    mod <- takeModifier
    case mod of
        Nothing -> pure Silent
        Just mod' -> zoomRPN mod'.runModSolo
