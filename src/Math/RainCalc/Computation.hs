module Math.RainCalc.Computation (
    Computation,
    terminate,
    push,
    pop,
    dup,
    swap,
    clear,
)
where

import Control.Monad.State.Strict as StateM
import Math.RainCalc.RPNState qualified as S

type Computation = StateM.State S.RPNState

terminate :: Computation ()
terminate = modify $ \state' -> state'{S.terminate_ = True}

push :: Double -> Computation ()
push = modify . S.push_

pop :: Int -> Computation [Double]
pop n = do
    stack <- gets S.stack_
    modify $ S.smap (drop n)
    return $ take n stack

dup :: Computation ()
dup = modify S.dup_

swap :: Computation ()
swap = modify S.swap_

clear :: Computation ()
clear = modify S.clear_
