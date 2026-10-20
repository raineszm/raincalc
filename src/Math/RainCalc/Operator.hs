{-# LANGUAGE OverloadedRecordDot #-}

module Math.RainCalc.Operator (
    operators,
    evalOperator,
)
where

import Control.Monad.State.Class (gets, modify)
import Math.RainCalc.Action (Action (..), OperatorSpec (..))
import Math.RainCalc.CalcError (CalcError (..))
import Math.RainCalc.Computation (Computation, pop, push)
import Math.RainCalc.Output (Output (..), silent)
import Math.RainCalc.RPNState (RPNState (..))

binary :: [String] -> (Double -> Double -> Double) -> Action OperatorSpec
binary names f =
    Action
        { names
        , actionSpec = OperatorSpec{arity = 2, runOp = action}
        , desc = ""
        }
  where
    action [x, y] = Right $ y `f` x
    action _ = Left $ StackUnderflow 2

binaryOperators :: [Action OperatorSpec]
binaryOperators =
    map
        (uncurry binary)
        [ (["+"], (+))
        , (["-"], (-))
        , (["/"], (/))
        , (["*"], (*))
        , (["^", "**"], (**))
        ]

buildOperator :: Int -> ([Double] -> Double) -> OperatorSpec
buildOperator arity f =
    OperatorSpec{arity = arity, runOp = Right . f}

unary :: [String] -> (Double -> Double) -> Action OperatorSpec
unary names f =
    Action
        { names
        , actionSpec = OperatorSpec{arity = 1, runOp = action}
        , desc = ""
        }
  where
    action [x] = Right $ f x
    action _ = Left $ StackUnderflow 1

unaryOperators :: [Action OperatorSpec]
unaryOperators =
    map
        (uncurry unary)
        [ (["sin", "s"], sin)
        , (["cos", "c"], cos)
        , (["tan", "t"], tan)
        , (["sinh", "sh"], sinh)
        , (["cosh", "ch"], cosh)
        , (["tanh", "th"], tanh)
        , (["atan"], atan)
        , (["acos"], acos)
        , (["asin"], asin)
        , (["exp"], exp)
        , (["atanh"], atanh)
        , (["acosh"], acosh)
        , (["asinh"], asinh)
        , (["exp"], exp)
        , (["ln", "log"], log)
        , (["deg", "todegrees"], toDegrees)
        , (["rad", "toradians"], toRadians)
        , (["factorial", "fact", "!"], factorial)
        , (["inv", "1/"], (1 /))
        , (["neg"], negate)
        , (["signum", "sgn", "sign"], signum)
        , (["abs", "||"], abs)
        , (["sqrt", "v"], sqrt)
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

foldOp :: [String] -> ([Double] -> Double) -> Action OperatorSpec
foldOp names f =
    Action
        { names
        , actionSpec = OperatorSpec{arity = -1, runOp = Right . f}
        , desc = ""
        }

foldOperators :: [Action OperatorSpec]
foldOperators =
    map (uncurry foldOp) [(["sum"], sum), (["prod"], product)]

constOp :: [String] -> Double -> Action OperatorSpec
constOp names value =
    Action
        { names
        , actionSpec = OperatorSpec{arity = -1, runOp = const $ Right value}
        , desc = ""
        }

constants :: [Action OperatorSpec]
constants =
    map (uncurry constOp) [(["e"], exp 1), (["pi"], pi)]

operators :: [Action OperatorSpec]
operators =
    concat [constants, unaryOperators, binaryOperators, foldOperators]

evalOperator :: OperatorSpec -> Computation Output
evalOperator op
    | arity' >= 0 = do
        args <- pop arity'
        case action args of
            Right result -> silent $ push result
            Left e -> pure $ ShowError e
    | otherwise = do
        stack <- gets stack_
        case action stack of
            Right result -> silent . modify $ \r ->
                r{stack_ = [result]}
            Left e -> pure $ ShowError e
  where
    action = op.runOp
    arity' = op.arity
