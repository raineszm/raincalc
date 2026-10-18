module Math.RainCalc.Operator (
    operators,
    evalOperator,
)
where

import Control.Monad (unless)
import Control.Monad.State.Lazy (modify)
import Data.List (intercalate)
import Data.Map qualified as Map
import Math.RainCalc.Action (Operator (..))
import Math.RainCalc.Computation (Computation, pop, push)
import Math.RainCalc.RPNState (smap)
import Math.RainCalc.Utils (expandWith)

binaryOperator :: (Double -> Double -> Double) -> Operator
binaryOperator f = Operator{arity = 2, runOp = action}
  where
    action ~[x, y] = y `f` x

map2nd :: (b -> c) -> [(a, b)] -> [(a, c)]
map2nd f = map g
  where
    g (x, y) = (x, f y)

binaryOperators :: [([String], Operator)]
binaryOperators =
    map2nd
        binaryOperator
        [ (["+"], (+))
        , (["-"], (-))
        , (["/"], (/))
        , (["*"], (*))
        , (["^", "**"], (**))
        ]

buildOperator :: Int -> ([Double] -> Double) -> Operator
buildOperator arity f =
    Operator{arity = arity, runOp = f}

unaryOperator :: (Double -> Double) -> Operator
unaryOperator f = buildOperator 1 (f . head)

unaryOperators :: [([String], Operator)]
unaryOperators =
    map2nd
        unaryOperator
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
    map2nd (buildOperator (-1)) [(["sum"], sum), (["prod"], product)]

constants :: [([String], Operator)]
constants =
    map2nd (buildOperator 0) [(["e"], const (exp 1)), (["pi"], const pi)]

operators :: [([String], Operator)]
operators =
    concat [constants, unaryOperators, binaryOperators, foldOperators]

evalOperator :: Operator -> Computation ()
evalOperator op
    | arity' >= 0 = do
        args <- pop arity'
        unless (length args < arity') (push $ action args)
    | otherwise = modify $ smap (pure . action)
  where
    action = runOp op
    arity' = arity op
