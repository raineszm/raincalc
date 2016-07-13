module Math.RainCalc.Registers where
import           Control.Monad.State.Lazy
import           Data.Char                (isAlpha, toLower)
import qualified Data.Map                 as Map

import qualified Math.RainCalc.RPNState   as S

setR :: Char -> Double -> S.Computation ()
setR r = modify . S.rmap . Map.insert r

getR :: Char -> S.Computation (Maybe Double)
getR r = gets $ Map.lookup r . S.registers_

isValid :: Char -> Bool
isValid = isAlpha . toLower

present :: S.RPNState -> String
present = concatMap presentPair . Map.toList . S.registers_
        where
                presentPair (k,v) = show k ++ ": " ++ show v ++ "\n"

