module Math.RainCalc.TestHelper where
	import Control.Monad.State.Lazy (evalState)
	import Math.RainCalc.RPNState

	top :: Computation a -> Double
	top c = head $ evalState (c >> (pop 1)) emptyState

