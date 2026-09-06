{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Game where
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

import Node
import qualified Constants  as C
import Pacman               as P
import Ghost                as G
import Helper
import Vec
import Modes



-- 2 options for Pacman movement (up, down, left, right):
-- 1) Hover your mouse over the black panel, then
-- left-click, right-click, scroll up or scroll down 
-- 2) Click movement buttons at the bottom
main :: IO ()
main = start pacmanGame



pacmanGame :: IO ()
pacmanGame = do
    ff          <- frame            [   text       := "Pacman"
                                        , bgcolor    := rgb3 C.black
                                        , resizeable := False
                                    ]
    t          <- timer ff          [   interval   := C.timerInterval ]      -- timer ticks every "interval" ms

    game        <- menuPane         [   text := "&Game" ]
    new         <- menuItem game    [   text := "&New\tCtrl+N", help := "New game" ]
    pauseItem   <- menuItem game    [   text      := "&Pause\tCtrl+P"
                                        , help      := "Pause game"
                                        , checkable := True
                                    ]
    menuLine game
    quit        <- menuQuit game    [   help := "Quit the game" ]
    set new                         [   on command := pacmanGame ]
    set pauseItem                   [   on command := set t [enabled :~ not] ]
    set quit                        [   on command := close ff ]


    pMain       <- panel ff         [   bgcolor := black ]
    pBottom     <- panel ff         [   bgcolor  := grey ]
    bUp         <- button pBottom   [   text := "↑", bgcolor := grey ]
    bDown       <- button pBottom   [   text := "↓", bgcolor := grey ]
    bLeft       <- button pBottom   [   text := "←", bgcolor := grey ]
    bRight      <- button pBottom   [   text := "→", bgcolor := grey ]
    let bSize = minsize (sz C.btnSizeX C.btnSizeY)
    set pBottom [   layout := margin 10 $ floatCentre (grid 0 0 [
                        [ label "", bSize $ widget bUp, label ""],
                        [ bSize $ widget bLeft, bSize $ widget bDown, bSize $ widget bRight] 
                    ]) 
                ]    
    set ff      [   menuBar := [game],
                    layout := column 0 [
                        minsize (sz C.screenWidth C.mainPanelHeight) $ widget pMain
                        , minsize (sz C.screenWidth (-1)) $ widget pBottom
                    ]
                ]



    nodesMap <- getNodesMap
    let nodes               = getNodeGroupFromMap nodesMap
        pacmanStartKey      = getSafeStartKeyFromNodes nodesMap C.pacmanStartNodeKey
        pacmanStartPosition = mkVecFromPoint pacmanStartKey
        ghostStartKey       = getSafeStartKeyFromNodes nodesMap C.ghostStartNodeKey
        ghostStartPosition  = mkVecFromPoint ghostStartKey


    let networkDescription :: MomentIO ()
        networkDescription = mdo
            etick               <- event0 t command
            bModeTimer          <- accumB scatter (pure updateMode <@ etick)
            let bMode           = (\(_, _, mode) -> mode) <$> bModeTimer
            eClickButtonUp      <- event0 bUp  command
            eClickButtonDown    <- event0 bDown  command
            eClickButtonLeft    <- event0 bLeft  command
            eClickButtonRight   <- event0 bRight  command
            (eMouse :: Event EventMouse) <- event1 pMain mouse
            let eLeftClick   :: Event (Point, Modifiers) = filterJust (leftDown     <$> eMouse)
                eRightClick  :: Event (Point, Modifiers) = filterJust (rightDown    <$> eMouse)
                eScrollUp    :: Event (Point, Modifiers) = filterJust (mouseWheelUp <$> eMouse)
                eScrollDown  :: Event (Point, Modifiers) = filterJust (mouseWheelDown <$> eMouse)
            eUserMove           <- accumE "s" $ unions [ 
                    const "u" <$ eClickButtonUp
                    , const "u" <$ eScrollUp 
                    , const "d" <$ eClickButtonDown
                    , const "d" <$ eScrollDown
                    , const "l" <$ eClickButtonLeft
                    , const "l" <$ eLeftClick
                    , const "r" <$ eClickButtonRight
                    , const "r" <$ eRightClick
                ]


            (bUserDir :: Behavior String) <- stepper C.startDirection eUserMove
            (bPacmanState :: Behavior (Vec Float, Point, Point, String))
                <- accumB
                    (pacmanStartPosition, pacmanStartKey, pacmanStartKey, C.startDirection)
                    ((P.updateState nodesMap) <$> bUserDir <@ etick)
            let bPacmanPosInt = getIntPositionFromState <$> bPacmanState :: Behavior Point
                bGhostGoal = getGoalFromMode <$> bMode <*> bPacmanPosInt :: Behavior Point
            (bGhostState :: Behavior (Vec Float, Point, Point, String))
                <- accumB
                    (ghostStartPosition, ghostStartKey, ghostStartKey, C.startDirection)
                    ((G.updateState nodesMap) <$> bGhostGoal <@ etick)
            let
                bGhostPosInt = getIntPositionFromState <$> bGhostState :: Behavior Point
                bNodes    = pure nodes

            bpaint <- stepper (\_dc _ -> return ()) $ (drawGameState <$> bPacmanPosInt <*> bGhostPosInt <*> bGhostGoal <*> bNodes) <@ etick
            sink pMain [on paint :== bpaint]
            reactimate $ repaint pMain <$ etick
    network <- compile networkDescription
    actuate network


drawGameState :: Point -> Point -> Point -> NodeGroup -> DC a -> b -> IO ()
drawGameState pacmanPos ghostPos ghostGoal nodes dc _view = do
    drawNodes       nodes       dc
    drawPortalNodes nodes       dc
    P.drawPacman    pacmanPos   dc
    G.drawGhost     ghostPos    dc
    G.drawGhostGoal ghostGoal   dc

getIntPositionFromState :: (Vec Float, Point, Point, String) -> Point
getIntPositionFromState (Vec x y ,_ ,_ ,_) = point (round x) (round y)
