{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Game where

import Graphics.UI.WX hiding (Event)
import qualified Graphics.UI.WXCore as WXCore
import Reactive.Banana
import Reactive.Banana.WX
import qualified Data.HashMap.Strict as HM

import Node
import qualified Constants as C
import Pacman as P
import Ghost as G
import Helper as H
import Vec
import Collectable
import Modes

data GameStatus = Running | Won | Lost
    deriving (Eq, Show)

-- Status
data World = World
    { pacmanState    :: (Vec Float, Point, Point, String)
    , ghostState     :: (Vec Float, Point, Point, String)
    , collectables   :: [Collectable]
    , status         :: GameStatus
    }

drawMaze :: NodeGroup -> DC a -> b -> IO ()
drawMaze nodes dc _ = do
    drawNodes nodes dc

main :: IO ()
main = start pacmanGame

pacmanGame :: IO ()
pacmanGame = do
    ff <- frame
        [ text := "Pacman"
        , bgcolor := H.rgb3 C.black
        , resizeable := False
        ]
    t <- timer ff [interval := C.timerInterval]

    -- Menu
    game <- menuPane [text := "&Game"]
    new <- menuItem game [text := "&New\tCtrl+N", help := "New game"]
    pauseItem <- menuItem game [text := "&Pause\tCtrl+P", help := "Pause game", checkable := True]
    menuLine game
    quit <- menuQuit game [help := "Quit the game"]
    set new [on command := pacmanGame]
    set pauseItem [on command := set t [enabled :~ not]]
    set quit [on command := close ff]

    -- Panels
    pMain <- panel ff [bgcolor := black]
    pBottom <- panel ff [bgcolor := grey]
    bUp <- button pBottom [text := "↑", bgcolor := grey]
    bDown <- button pBottom [text := "↓", bgcolor := grey]
    bLeft <- button pBottom [text := "←", bgcolor := grey]
    bRight <- button pBottom [text := "→", bgcolor := grey]
    let bSize = minsize (sz C.btnSizeX C.btnSizeY)
    set pBottom [ layout := margin 10 $ floatCentre $
        grid 0 0
            [ [label "", bSize $ widget bUp, label ""]
            , [bSize $ widget bLeft, bSize $ widget bDown, bSize $ widget bRight]
            ]
        ]
    set ff [ menuBar := [game]
           , layout := column 0
                [ minsize (sz C.screenWidth C.mainPanelHeight) $ widget pMain
                , minsize (sz C.screenWidth (-1)) $ widget pBottom
                ]
           ]

    -- Maze and nodes
    nodesMap <- getNodesMap
    let nodes = getNodeGroupFromMap nodesMap
        pacmanStartKey = getSafeStartKeyFromNodes nodesMap C.pacmanStartNodeKey
        pacmanStartPosition = mkVecFromPoint pacmanStartKey
        ghostStartKey = getSafeStartKeyFromNodes nodesMap C.ghostStartNodeKey
        ghostStartPosition = mkVecFromPoint ghostStartKey

    -- FRP Event network
    let networkDescription :: MomentIO ()
        networkDescription = mdo
            -- clock tick
            eTick <- event0 t command

            -- Mode timer and mode behavior
            bModeTimer <- accumB scatter (pure updateMode <@ eTick)
            let bMode = (\(_,_,m) -> m) <$> bModeTimer


            -- User input
            eClickButtonUp      <- event0 bUp command
            eClickButtonDown    <- event0 bDown command
            eClickButtonLeft    <- event0 bLeft command
            eClickButtonRight   <- event0 bRight command
            (eMouse :: Event EventMouse)    <- event1 pMain mouse
            let eLeftClick  = filterJust (leftDown  <$> eMouse)
                eRightClick = filterJust (rightDown <$> eMouse)
                eScrollUp   = filterJust (mouseWheelUp   <$> eMouse)
                eScrollDown = filterJust (mouseWheelDown <$> eMouse)
            eUserMove <- accumE "s" $ unions
                [ const "u" <$ eClickButtonUp
                , const "u" <$ eScrollUp
                , const "d" <$ eClickButtonDown
                , const "d" <$ eScrollDown
                , const "l" <$ eClickButtonLeft
                , const "l" <$ eLeftClick
                , const "r" <$ eClickButtonRight
                , const "r" <$ eRightClick
                ]
            bUserDirection <- stepper C.startDirection eUserMove

            -- Pacman/ghost state + collectables, all wrapped in World
            let initialWorld = World
                    { pacmanState  = (pacmanStartPosition, pacmanStartKey, pacmanStartKey, C.startDirection)
                    , ghostState   = (ghostStartPosition, ghostStartKey, ghostStartKey, C.startDirection)
                    , collectables = collectablesFromNodesAndPaths nodes
                    , status       = Running
                    }
                bGhostGoal :: Behavior Point                = getGoalFromMode <$> bMode <*> bPacmanPosInt
                bUpdateWorld :: Behavior (World -> World)   = updateWorld nodesMap <$> bUserDirection <*> bGhostGoal

            (bWorld :: Behavior World) <- accumB initialWorld (bUpdateWorld <@ eTick)

            let bPacmanPosInt  = getIntPositionFromState . pacmanState <$> bWorld
                bGhostPosInt   = getIntPositionFromState . ghostState  <$> bWorld
                bCollectables  = collectables <$> bWorld
                bGameStatus    = status      <$> bWorld
                bNodes         = pure nodes

            bpaint <- stepper (\_ _ -> return ()) $
                (drawGameState
                   <$> bPacmanPosInt
                   <*> bGhostPosInt
                   <*> bGhostGoal
                   <*> bNodes
                   <*> bCollectables
                   <*> bGameStatus
                ) <@ eTick

            -- this is now parsed as two monadic actions
            sink pMain [on paint :== bpaint]
            reactimate $ repaint pMain <$ eTick

    network <- compile networkDescription
    actuate network


------------------------------------------------------------

getIntPositionFromState :: (Vec Float, Point, Point, String) -> Point
getIntPositionFromState (Vec x y, _, _, _) = point (round x) (round y)

drawGameState :: Point -> Point -> Point -> NodeGroup -> [Collectable] -> GameStatus -> DC a -> b -> IO ()
drawGameState pacmanPos ghostPos ghostGoal nodes collectablesList gameStatus dc _ = do
    drawMaze nodes dc ()
    mapM_ (\c -> drawCollectable c dc) (filter (not . isCollected) collectablesList)
    P.drawPacman pacmanPos dc
    G.drawGhost ghostPos dc
    case gameStatus of
        Won -> do
            WXCore.dcSetTextForeground dc green
            drawText dc "YOU WIN!" (pt 120 0) customFont
        Lost -> do
            WXCore.dcSetTextForeground dc red
            drawText dc "GAME OVER" (pt 96 0) customFont
        _ -> G.drawGhostGoal ghostGoal dc
    where
        customFont =    [ fontSize   := 32
                        , fontWeight := WeightBold
                        , fontUnderline := False
                        ]

updateWorld :: HM.HashMap Point Node
            -> String
            -> Point
            -> World
            -> World
updateWorld nodesMap userDirection ghostGoal world
    | status world == Won  = world
    | status world == Lost = world
    | otherwise =
        let
            newPacState = P.updateState nodesMap userDirection (pacmanState world)
            pacPosInt   = getIntPositionFromState newPacState
            newGhostState = G.updateState nodesMap ghostGoal (ghostState world)
            ghostPosInt   = getIntPositionFromState newGhostState
            newCollects = updateCollectables pacPosInt (collectables world)
            pacLost = H.isAlmostEqual pacPosInt ghostPosInt C.caughtThresh
            pacWon  = all isCollected newCollects
            newStatus | pacLost   = Lost
                      | pacWon    = Won
                      | otherwise = Running
        in
        world { pacmanState  = newPacState
              , ghostState   = newGhostState
              , collectables = newCollects
              , status       = newStatus
              }


-- TODO: delete
    -- -- WXCore.dcSetFont dc bigFont
    -- WXCore.dcSetTextForeground dc green
    -- drawText dc "YOU WIN!"   (pt 192 64) []
    -- WXCore.dcSetTextForeground dc red
    -- drawText dc "GAME OVER"  (pt 192 64) []

    -- bigFont <- WXCore.fontCreate
    --     [ WXCore.fontSize   := 48
    --     , WXCore.fontWeight := WXCore.fontWeightBold
    --     ]
    -- WXCore.dcSetFont dc bigFont

-- TODO: These are presentation examples (information comments)
-- let initialWorld :: World = World
--         { pacmanState  = (pacmanStartPosition, ... )
--         , ghostState   = (ghostStartPosition, ...)
--         , collectables = ...
--         , status       = Running
--         } 
-- bUpdateWorld :: Behavior (World -> World) <- ... 
-- 
-- let bMode = ... :: Behavior Int
-- <$> ::          (a -> b) -> Behavior a -> Behavior b
-- <*> :: Behavior (a -> b) -> Behavior a -> Behavior b
-- <@  :: Behavior b -> Event a -> Event b