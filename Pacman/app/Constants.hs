module Constants where
import Graphics.UI.WXCore



-- timer
timerInterval :: Int
timerInterval = 16



-- UI element sizes
tileWidth :: Int
tileWidth = 16

tileHeight :: Int
tileHeight = 16

nRows :: Int
nRows = 36

nCols :: Int
nCols = 28

screenWidth :: Int
screenWidth = nCols*tileWidth

mainPanelHeight :: Int
mainPanelHeight = nRows*tileHeight

mainPanelSize :: (Int, Int)
mainPanelSize = (screenWidth, mainPanelHeight)

btnSizeX :: Int
btnSizeX = 120

btnSizeY :: Int
btnSizeY = 100



--colors
black :: (Int, Int, Int)
black = (0, 0, 0)

white :: (Int, Int, Int)
white = (255, 255, 255)

red :: (Int, Int, Int)
red = (255, 0, 0)

yellow :: (Int, Int, Int)
yellow = (255, 255, 0)

pink :: (Int, Int, Int)
pink = (255, 192, 203)



-- Entity attributes
-- time in seconds that passes between each tick
deltaTime :: Float
deltaTime = (fromIntegral timerInterval)*0.001



-- Pacman attributes
pacmanStartPos :: (Float, Float)
pacmanStartPos = (200.0, 400.0)

pacmanNormalStepSize :: Float
pacmanNormalStepSize = ((fromIntegral tileWidth) * 11.0) * ((fromIntegral timerInterval)*0.001) :: Float

-- speed measured in tiles / second
pacmanSpeed :: Float
pacmanSpeed = (fromIntegral tileWidth) * 11.0

pacmanSize :: Int
pacmanSize = 10

pacmanStartNodeKey :: Point
pacmanStartNodeKey = point 16 64

startDirection :: String
startDirection = "s"

caughtThresh :: Int
caughtThresh = max (ceiling (pacmanSpeed * deltaTime)) pacmanSize



-- Ghost attributes
ghostStartNodeKey :: Point
ghostStartNodeKey = point (15 * tileWidth) (14 * tileHeight)

ghostSpeed :: Float
ghostSpeed = (fromIntegral tileWidth) * 10

ghostSize :: Int
ghostSize = 10



-- Mode attributes
scatter :: Int
scatter = 0

scatterGoal :: Point
scatterGoal = point 32 16

chase :: Int
chase = 1

scatterLength :: Int
scatterLength = 7000

chaseLength :: Int
chaseLength = 14000

freight :: Int
freight = 2

spawn :: Int
spawn = 3



-- Node attributes
mazeNodeSymbols :: [Char]
mazeNodeSymbols = ['+']

mazePathSymbols :: [Char]
mazePathSymbols = ['.']

portalTile1 :: (Int, Int)
portalTile1 = (0,17)

portalTile2 :: (Int, Int)
portalTile2 = (27,17)



-- Collectable attributes
collectableThresh :: Int
collectableThresh = pacmanSize + collectableSize

collectableSize :: Int
collectableSize = 5



-- Vec attributes
thresh :: Float
thresh = 0.9
