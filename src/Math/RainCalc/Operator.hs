module Math.RainCalc.Operator
  ( operators,
    evalOperator,
    Operator (..),
  )
where

import Control.Monad (unless)
import Control.Monad.State.Lazy (modify)
import Data.List (intercalate)
import Data.Map qualified as Map
import Math.RainCalc.Computation (Computation, pop, push)
import Math.RainCalc.RPNState (smap)
import Math.RainCalc.Utils (expandWith)

data Operator = Operator
  { arity_ :: Int,
    action_ :: [Double] -> Double,
    help_ :: String
  }

binaryOperator :: [String] -> (Double -> Double -> Double) -> Operator
binaryOperator names f = Operator {arity_ = 2, action_ = action, help_ = help}
  where
    action ~[x, y] = y `f` x
    help = simpleHelp mainName 2
    mainName = head names

binaryOperators :: [(String, Operator)]
binaryOperators =
  expandWith
    binaryOperator
    [ (["+"], (+)),
      (["-"], (-)),
      (["/"], (/)),
      (["*"], (*)),
      (["^", "**"], (**))
    ]

simpleHelp :: String -> Int -> String
simpleHelp name (-1) = "[... ] -> [" ++ name ++ "(...)]"
simpleHelp name 0 = "[... ] -> [... " ++ name ++ "]"
simpleHelp name 1 = "[... x] -> [... " ++ name ++ "(x)]"
simpleHelp name 2 = "[... x, y] -> [... x " ++ name ++ " y]"
simpleHelp name arity =
  "[... " ++ vars ++ "] -> [... " ++ name ++ "(" ++ vars ++ ")]"
  where
    vars = intercalate ", " $ map (('x' :) . show) [1 .. arity]

buildOperator :: Int -> [String] -> ([Double] -> Double) -> Operator
buildOperator arity names f =
  Operator {arity_ = arity, action_ = f, help_ = simpleHelp (head names) arity}

unaryOperator :: [String] -> (Double -> Double) -> Operator
unaryOperator names f = buildOperator 1 names (f . head)

unaryOperators :: [(String, Operator)]
unaryOperators =
  expandWith
    unaryOperator
    [ (["sin", "s"], sin),
      (["cos", "c"], cos),
      (["tan", "t"], tan),
      (["sinh", "sh"], sinh),
      (["cosh", "ch"], cosh),
      (["tanh", "th"], tanh),
      (["atan"], atan),
      (["acos"], acos),
      (["asin"], asin),
      (["exp"], exp),
      (["atanh"], atanh),
      (["acosh"], acosh),
      (["asinh"], asinh),
      (["exp"], exp),
      (["ln", "log"], log),
      (["deg", "todegrees"], toDegrees),
      (["rad", "toradians"], toRadians),
      (["factorial", "fact", "!"], factorial),
      (["inv", "1/"], (1 /)),
      (["neg"], negate),
      (["signum", "sgn", "sign"], signum),
      (["abs", "||"], abs),
      (["sqrt", "v"], sqrt)
    ]

toDegrees :: Double -> Double
toDegrees = (180 *) . (/ pi)

toRadians :: Double -> Double
toRadians = (pi *) . (/ 180)

factorial :: Double -> Double
factorial 0 = 1
factorial n
  | n < 0 = 0 / 0
  | otherwise = fromIntegral $ product [1 .. (round n :: Integer)]

foldOperators :: [(String, Operator)]
foldOperators =
  expandWith (buildOperator (-1)) [(["sum"], sum), (["prod"], product)]

constants :: [(String, Operator)]
constants =
  expandWith (buildOperator 0) [(["e"], const (exp 1)), (["pi"], const pi)]

operators :: Map.Map String Operator
operators =
  Map.fromList $
    concat [constants, unaryOperators, binaryOperators, foldOperators]

isOperator :: String -> Bool
isOperator = (`Map.member` operators)

evalOperator :: Operator -> Computation ()
evalOperator op
  | arity >= 0 = do
      args <- pop arity
      unless (length args < arity) (push $ action args)
  | otherwise = modify $ smap (pure . action)
  where
    action = action_ op
    arity = arity_ op
