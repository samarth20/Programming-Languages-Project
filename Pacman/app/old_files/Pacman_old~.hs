{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Pacman_old where
import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore as WXCore
import Reactive.Banana
import Reactive.Banana.WX
import System.Random

import Paths (getDataFile)
import Vector
import qualified Constants as C


-- constants
-- TODO: put this into "Constants.hs"
height, width, diameter :: Int
height   = 300
width    = 300
diameter = 24

winW     = 448
panelH   = 576
btnSize  = 80
gapUp    = (winW - btnSize) `div` 2 -- 184
gapAr    = (winW - btnSize*3) `div` 2 -- 104
winH     = panelH + btnSize*2         -- 736


v1 = mkVector 5.0 6.0 7.0

chance   :: Double
chance   = 0.1

rock, burning, ship :: Bitmap ()
rock    = bitmap $ getDataFile "rock.ico"
burning = bitmap $ getDataFile "burning.ico"
ship    = bitmap $ getDataFile "ship.ico"

explode :: WXCore.Sound ()
explode = sound $ getDataFile "explode.wav"

main :: IO ()
main = start pacman



pacman :: IO ()
pacman = do
    ff <- frame [ text       := "Pacman"
                , bgcolor    := black -- black -- TODO: undo -- rgb (255) (255) (200) 
                -- , resizeable := False       -- TODO: undo
                ]
    status <- statusField [text := "Welcome to pacman"]
    set ff [statusBar := [status]]


    let mkBtn :: String -> IO (Button ())
        mkBtn label = button ff
             [ text    := label
            --  , minsize := sz btnSize btnSize
             , bgcolor := grey
             ]
    bLeft    <- mkBtn "←"        -- button ff   [text := "←"]
    bUp      <- mkBtn "↑"        -- button ff   [text := "↑"]
    bRight   <- mkBtn "→"        -- button ff   [text := "→"]
    bDown    <- mkBtn "↓"        -- button ff   [text := "↓"]
    -- bQuit          <- button ff   [text := "Quit", on command := close ff]
    

    t  <- timer ff [ interval   := 50 ]

    game  <- menuPane      [ text := "&Game" ]
    new   <- menuItem game [ text := "&New\tCtrl+N", help := "New game" ]
    pause <- menuItem game [ text      := "&Pause\tCtrl+P"
                           , help      := "Pause game"
                           , checkable := True
                           ]
    menuLine game
    quit  <- menuQuit game [help := "Quit the game"]

    set new   [on command := pacman]
    set pause [on command := set t [enabled :~ not]]
    set quit  [on command := close ff]

    -- set ff [menuBar := [game]]

    st <- staticText ff [ 
        text := "Hello StaticText!"
        , bgcolor := grey
     ]

    pMain     <- panel ff [ 
        -- bgcolor := black 
        ]
    pTop      <- panel ff
       [ bgcolor  := black
    --    , minsize  := sz (-1) 150 
       ]
    pBottom   <- panel ff 
           [ bgcolor  := grey
    --    , minsize  := sz (-1) 150
       ]

                     -- layout := minsize (sz winW panelH) ] -- []
    set ff [ menuBar := [game],
             layout := column 0
              [
                minsize (sz winW panelH) $ widget pMain,
                minsize (sz (-1) 150) $ widget pBottom
                -- column 5 [
                -- minsize (sz winW panelH) $ widget pBottom-- $ widget pMain
                -- , widget pBottom
                -- , floatCentre (widget pBottom)

                -- , widget st
                 ]
            ] --column 5 ]
                       -- [ -- widget pp, -- minsize (sz winW panelH) $
                       --   row 0 [ widget bUp, widget bUp,    widget bUp],
                       --   row 0 [ widget bLeft,  widget bDown, widget bRight]
                       -- ]]

    -- set ff [ layout  := minsize (sz C.screenwidth C.screenheight) $ widget pp ]          --
                        -- margin 10 $
                        -- column 20
                        --   [ widget pp,
                        --     row 1 [ widget bUp],
                        --     row 3 [ widget bLeft, widget bDown, widget bRight ]
                        --     ] ]

    -- column 5 [label "leftButtonLabel",widget leftButton]]
    -- set ff [ layout  := minsize (sz width height) $ widget pp ]
    set pMain [ on (charKey '-') := set t [interval :~ \i -> i * 2]
           , on (charKey 'z') := set t [interval :~ \i -> max 10 (div i 2)] -- todo: change back '+'
           ]
    set pBottom [ layout := column 2 [ -- margin 10 $ 
                    row 0 [ widget bUp],
                    row 0 [ widget bLeft,  widget bDown, widget bRight]
                ]
        ]

    -- event network
    let networkDescription :: MomentIO ()
        networkDescription = mdo
            -- timer
            etick  <- event0 t command

            -- keyboard events
            -- new comment
            ekey   <- event1 pMain keyboard
            reactimate (fmap (putStrLn . show) ekey)
            let eleft  = filterE ((== KeyLeft ) . keyKey) ekey
                eright = filterE ((== KeyRight) . keyKey) ekey

            -- ship position
            (bship :: Behavior Int)
                <- accumB (C.screenwidth `div` 2) $ unions
                    [ goLeft  <$ eleft
                    , goRight <$ eright
                    ]
            let
                goLeft  x = max 0          (x - 5)
                goRight x = min (C.screenwidth-30) (x + 5)

            -- rocks
            brandom <- fromPoll (randomRIO (0,1) :: IO Double)

            (brocks :: Behavior [Rock])
                <- accumB [] $ unions
                    [ advanceRocks <$ etick
                    , newRock      <$> filterE (< chance) (brandom <@ etick)
                    ]

            -- draw the game state
            bpaint <- stepper (\_dc _ -> return ()) $
                        (drawGameState <$> bship <*> brocks) <@ etick
            sink pMain [on paint :== bpaint]
            reactimate $ repaint pMain <$ etick

            -- status bar
            let bstatus :: Behavior String
                bstatus = (\r -> "rocks: " ++ show (length r)) <$> brocks
            sink status [text :== bstatus]

    network <- compile networkDescription
    actuate network


-- rock logic
type Position = Point2 Int
type Rock     = [Position] -- lazy list of future y-positions

newRock :: Double -> [Rock] -> [Rock]
newRock r rs = (track . floor $ fromIntegral C.screenwidth * r / chance) : rs

track :: Int -> Rock
track x = [point x (y - diameter) | y <- [0, 6 .. C.screenheight + 2 * diameter]]

advanceRocks :: [Rock] -> [Rock]
advanceRocks = filter (not . null) . map (drop 1)



-- draw game state
drawGameState :: Int -> [Rock] -> DC a -> b -> IO ()
drawGameState ship rocks dc _view = do
    let
        shipLocation = point ship (C.screenheight - 2 * diameter)
        positions    = map head rocks
        collisions   = map (collide shipLocation) positions

    drawShip dc shipLocation
    mapM (drawRock dc) (zip positions collisions)

    when (or collisions) (play explode)

collide :: Position -> Position -> Bool
collide pos0 pos1 =
    let distance = vecLength (vecBetween pos0 pos1)
    in distance <= fromIntegral diameter

drawShip :: DC a -> Point -> IO ()
drawShip dc pos = drawBitmap dc ship pos True []

drawRock :: DC a -> (Point, Bool) -> IO ()
drawRock dc (pos, collides) =
    let rockPicture = if collides then burning else rock
    in drawBitmap dc rockPicture pos True []
