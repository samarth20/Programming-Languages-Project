module Modes where
import Graphics.UI.WXCore

import qualified Constants as C



updateMode :: (Int, Int, Int) -> (Int, Int, Int)
updateMode (timer, time, mode) = 
    let timer' = timer + C.timerInterval 
    in if timer' >= time 
        then
            if mode == C.scatter
                then chase
            else if mode == C.chase
                then scatter
            else (0, time, mode)
        else (timer', time, mode)

chase :: (Int, Int, Int)
chase = (0, C.chaseLength, C.chase)

scatter :: (Int, Int, Int)
scatter = (0, C.scatterLength, C.scatter)

getGoalFromMode :: Int -> Point -> Point
getGoalFromMode mode pacmanPosition =
    if mode == C.chase
        then pacmanPosition
        else C.scatterGoal
