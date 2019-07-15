module RTM where

import Control.Monad.State.Strict
import System.Random

type RTM = StateT StdGen IO

rtmR :: Random a => (a, a) -> RTM a
rtmR range = state (randomR range)

runRTM :: Double -> RTM a -> IO a
runRTM gen prog = let gen' = mkStdGen $ floor gen in evalStateT prog gen'
