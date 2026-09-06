module Ghost where
import Graphics.UI.WX as WX
import Graphics.UI.WXCore as WXCore
import qualified Data.HashMap.Strict as HM

import Node
import Vec
import Entity as E
import qualified Constants as C



updateState :: HM.HashMap Point Node -> Point -> (Vec Float, Point, Point, String) -> (Vec Float, Point, Point, String)
updateState hashMap goal (currentPosition, currentNodeKey, targetNodeKey, currentDirection) =
    let
        newPosition = currentPosition + scaleVec2 C.ghostSpeed C.deltaTime (getDirectionVec currentDirection)
        currentNodePosition = mkVecFromPoint currentNodeKey
        targetNodePosition  = mkVecFromPoint targetNodeKey
    in
        if isTargetOvershot newPosition currentNodePosition targetNodePosition
        then
            let currentNodeKey'                     = getCurrentNodeUpdate targetNodeKey hashMap
                newPosition'                        = mkVecFromPoint currentNodeKey'
                (currentDirection', targetNodeKey') = getNewTarget hashMap currentNodeKey' currentDirection goal
            in  (newPosition', currentNodeKey', targetNodeKey', currentDirection')
        else
            (newPosition, currentNodeKey, targetNodeKey, currentDirection)

getNewTarget :: HM.HashMap Point Node -> Point -> String -> Point -> (String, Point)
getNewTarget hashMap currentNodeKey currentDirection goal =
    case HM.lookup currentNodeKey hashMap of
        Nothing     -> ("s", currentNodeKey)
        Just currentNode   ->
            let directions  = getPossibleDirections currentDirection
                mDirectionNeighbors = map (\direction -> (direction, getMaybeNeighborFromDirection currentNode direction)) directions
                neighborDistances =
                    map (\(direction, mPoint) ->
                            case mPoint of
                                Just p  -> (direction, p, getSquaredDistance goal p)
                                Nothing -> error "Unexpected Nothing in neighborDistances despite filtering"
                        ) $
                    filter (\(_, mPoint) -> mPoint /= Nothing) mDirectionNeighbors
            in
                if (length neighborDistances) > 0 
                    then 
                        let (dir, p, _) = 
                                foldl1 (\(dir1, p1, dist1) (dir2, p2, dist2) -> 
                                    if dist2 < dist1
                                        then (dir2, p2, dist2)
                                        else (dir1, p1, dist1)) neighborDistances
                        in
                            (dir, p)
                    else ("s", currentNodeKey)

getPossibleDirections :: String -> [String]
getPossibleDirections currentDirection =
    let oppositeDirection = getOppositeDirection currentDirection
    in filter (\direction -> oppositeDirection /= direction) ["u", "d", "l", "r"]

getOppositeDirection :: String -> String
getOppositeDirection direction =
    case direction of
        "u" -> "d"
        "d" -> "u"
        "l" -> "r"
        "r" -> "l"
        _   -> direction

getSquaredDistance :: Point -> Point -> Int
getSquaredDistance (Point x1 y1) (Point x2 y2) = (dx * dx) + (dy * dy)
  where
    dx = x1 - x2
    dy = y1 - y2

getSignificantDimension :: Point -> String -> Int
getSignificantDimension (Point x y) direction =
    case direction of
        "u" -> x
        "d" -> x
        "l" -> y
        "r" -> y
        _   -> error ("Unexpected direction '" ++ direction ++ "' given.")



drawGhost :: Point -> DC a -> IO ()
drawGhost (Point x y) dc = 
    do
        set dc [ brushColor := red
                , brushKind  := BrushSolid
                , penColor := red
                , penKind  := PenSolid
                , penWidth := 1
                ]
        WXCore.dcDrawCircle dc (pt x y) C.ghostSize

drawGhostGoal :: Point -> DC a -> IO ()
drawGhostGoal (Point x y) dc = 
    do
        set dc [ brushColor := red
               , brushKind  := BrushTransparent
               , penColor := red
               , penKind  := PenSolid
               , penWidth := 2
               ]
        let l = 10
        WXCore.dcDrawLine dc (pt (x - l) (y - l)) (pt (x + l) (y + l))
        WXCore.dcDrawLine dc (pt (x - l) (y + l)) (pt (x + l) (y - l))
