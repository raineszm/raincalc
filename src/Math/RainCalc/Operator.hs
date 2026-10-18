module Math.RainCalc.Operator (
    operators,
    evalOperator,
)
where

import Control.Monad.State.Class (gets, modify)
import Math.RainCalc.Action (Operator (..))
import Math.RainCalc.CalcError (CalcError (..))
import Math.RainCalc.Computation (Computation, pop, push)
import Math.RainCalc.Output (Output (..), silent)
import Math.RainCalc.RPNState (RPNState (..))

binaryOperator :: (Double -> Double -> Double) -> Operator
binaryOperator f = Operator{arity = 2, runOp = action}
  where
    action [x, y] = Right $ y `f` x
    action _ = Left $ StackUnderflow 2

binaryOperators :: [([String], Operator)]
binaryOperators =
    map
        (fmap binaryOperator)
        [ (["+"], (+))
        , (["-"], (-))
        , (["/"], (/))
        , (["*"], (*))
        , (["^", "**"], (**))
        ]

buildOperator :: Int -> ([Double] -> Double) -> Operator
buildOperator arity f =
    Operator{arity = arity, runOp = Right . f}

unaryOperator :: (Double -> Double) -> Operator
unaryOperator f = Operator{arity = 1, runOp = action}
  where
    action [x] = Right $ f x
    action _ = Left $ StackUnderflow 1

unaryOperators :: [([String], Operator)]
unaryOperators =
    map
        (fmap unaryOperator)
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

foldOperators :: [([String], Operator)]
foldOperators =
    map (fmap $ buildOperator (-1)) [(["sum"], sum), (["prod"], product)]

constants :: [([String], Operator)]
constants =
    map (fmap $ buildOperator 0) [(["e"], const (exp 1)), (["pi"], const pi)]

operators :: [([String], Operator)]
operators =
    concat [constants, unaryOperators, binaryOperators, foldOperators]

evalOperator :: Operator -> Computation Output
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
    action = runOp op
    arity' = arity op
