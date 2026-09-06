module Collectable
  ( Collectable(..)
  , getCollectables
  , updateCollectables
  , drawCollectable
  , collectablesFromNodesAndPaths
  ) where

import Node (NodeGroup(..), getValue, getU, getD, getL, getR)
import Data.List (nub)
import Graphics.UI.WX
import qualified Constants as C
import Helper as H



data Collectable = Collectable
  { collectablePos :: Point
  , isCollected    :: Bool
  } deriving (Show, Eq)



-- Example list, adjust positions as you like:
getCollectables :: [Collectable]
getCollectables =
  [ Collectable (point 5  7) False
  , Collectable (point 10 3) False
  , Collectable (point 15 9) False
  ]

-- Update collectables: if Pacman is on one, mark it collected
updateCollectables :: Point -> [Collectable] -> [Collectable]
updateCollectables pacPos = map (\c -> if not (isCollected c) && H.isAlmostEqual (collectablePos c) pacPos C.collectableThresh
                                         then c { isCollected = True }
                                         else c)

pointsAlongEdge :: (Int, Int) -> (Int, Int) -> Int -> [Point]
pointsAlongEdge (x1, y1) (x2, y2) tileWidth
  | x1 == x2 = -- Vertical
      let yStart = min y1 y2
          yEnd = max y1 y2
          dist = yEnd - yStart
          nSpaces = ceiling ((fromIntegral dist :: Double) / (2 * fromIntegral tileWidth))
          spacing = dist `div` nSpaces
      in if nSpaces < 2 then []
         else [ Point x1 y | i <- [1 .. nSpaces - 1], let y = yStart + i * spacing, y < yEnd ]
  | y1 == y2 = -- Horizontal
      let xStart = min x1 x2
          xEnd = max x1 x2
          dist = xEnd - xStart
          nSpaces = ceiling ((fromIntegral dist :: Double) / (2 * fromIntegral tileWidth))
          spacing = dist `div` nSpaces
      in if nSpaces < 2 then []
         else [ Point x y1 | i <- [1 .. nSpaces - 1], let x = xStart + i * spacing, x < xEnd ]
  | otherwise = []

collectablesFromNodesAndPaths :: NodeGroup -> [Collectable]
collectablesFromNodesAndPaths (NodeGroup nodes) =
  let
    -- Collect collectables at each node
    nodePoints = [ getValue node | node <- nodes]
    -- For each connection, collect points along the edge
    edgePoints =
      concat [ pointsAlongEdge (x1, y1) (x2, y2) (C.tileWidth)
               | node <- nodes
               , neighborFunc <- [getU, getD, getL, getR]
               , Just (Point x2 y2) <- [neighborFunc node]
               , let Point x1 y1 = getValue node
               , (x1, y1) < (x2, y2) -- avoid double-counting
             ]
    allPoints = nub (nodePoints ++ edgePoints)
  in [ Collectable p False | p <- allPoints ]



-- Draw a collectable
drawCollectable :: Collectable -> DC a -> IO ()
drawCollectable c dc =
  if isCollected c then pure () else do
    let p = collectablePos c
    set dc  [ brushColor := H.rgb3 C.yellow
            , brushKind  := BrushSolid
            , penColor := H.rgb3 C.pink
            , penKind  := PenSolid
            , penWidth := 2
            ]
    circle dc p C.collectableSize []
