module Math.RainCalc.Registers where
import           Control.Monad.State.Lazy
import           Data.Char                (isAlpha, toLower)
import qualified Data.Map                 as Map

import qualified Math.RainCalc.Computation as C
import qualified Math.RainCalc.RPNState   as S

setR :: Char -> Double -> C.Computation ()
setR r = modify . setR_ r

setR_ :: Char -> Double -> S.RPNState -> S.RPNState
setR_ r = S.rmap . Map.insert r

getR :: Char -> C.Computation (Maybe Double)
getR = gets . getR_

getR_ :: Char -> S.RPNState -> Maybe Double
getR_ r = Map.lookup r . S.registers_

isValid :: Char -> Bool
isValid = isAlpha . toLower

present :: S.RPNState -> String
present = concatMap presentPair . Map.toList . S.registers_
        where
                presentPair (k,v) = show k ++ ": " ++ show v ++ "\n"
