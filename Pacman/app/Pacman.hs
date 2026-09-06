module Pacman where
import Graphics.UI.WX as WX hiding ( key )
import Graphics.UI.WXCore as WXCore
import qualified Data.HashMap.Strict as HM

import qualified Constants as C
import Node
import Vec
import Entity



updateState 
    :: HM.HashMap Point Node 
    -> String 
    -> (Vec Float, Point, Point, String) 
    -> (Vec Float, Point, Point, String)
updateState hashMap userDirection (currentPosition, currentNodeKey, targetNodeKey, currentDirection)
    | isOppositeDirection userDirection currentDirection =
        let newPosition         = currentPosition + scaleVec2 C.pacmanSpeed C.deltaTime (getDirectionVec userDirection)
            currentNodeKey'     = targetNodeKey
            targetNodeKey'      = currentNodeKey
            currentDirection'   = userDirection
        in  (newPosition, currentNodeKey', targetNodeKey', currentDirection')
    | otherwise =
        let newPosition         = currentPosition + scaleVec2 C.pacmanSpeed C.deltaTime (getDirectionVec currentDirection)
            currentNodePosition = mkVecFromPoint currentNodeKey
            targetNodePosition  = mkVecFromPoint targetNodeKey
        in
            if isTargetOvershot newPosition currentNodePosition targetNodePosition
            then
                let currentNodeKey'                     = getCurrentNodeUpdate targetNodeKey hashMap
                    newPosition'                        = mkVecFromPoint currentNodeKey'
                    (currentDirection', targetNodeKey') = getNewTarget hashMap currentNodeKey' userDirection currentDirection
                in  (newPosition', currentNodeKey', targetNodeKey', currentDirection')
            else
                (newPosition, currentNodeKey, targetNodeKey, currentDirection)

getNewTarget :: HM.HashMap Point Node -> Point -> String -> String -> (String, Point)
getNewTarget hashMap currentNodeKey userDirection currentDirection =
    let mUserDirectionTargetNodeKey     = getMaybeTargetNodeKeyFromDirection hashMap userDirection    currentNodeKey
        mCurrentDirectionTargetNodeKey  = getMaybeTargetNodeKeyFromDirection hashMap currentDirection currentNodeKey
    in case mUserDirectionTargetNodeKey of
         Just key  -> (userDirection, key)
         Nothing -> case mCurrentDirectionTargetNodeKey of
                      Just key  -> (currentDirection, key)
                      Nothing -> ("s",  currentNodeKey)

getMaybeTargetNodeKeyFromDirection :: HM.HashMap Point Node -> String -> Point-> Maybe Point
getMaybeTargetNodeKeyFromDirection hashMap direction key =
    case HM.lookup key hashMap of
      Nothing   -> Nothing
      Just node -> getMaybeNeighborFromDirection node direction

isOppositeDirection :: String -> String -> Bool
isOppositeDirection userDirection currentDirection =
    case (userDirection, currentDirection) of
        ("u", "d") -> True
        ("d", "u") -> True
        ("l", "r") -> True
        ("r", "l") -> True
        _          -> False



drawPacman :: Point -> DC a -> IO ()
drawPacman (Point x y) dc = 
    do
        set dc [ brushColor := yellow
                , brushKind  := BrushSolid
                , penColor := yellow
                , penKind  := PenSolid
                , penWidth := 1
                ]
        WXCore.dcDrawCircle dc (pt x y) C.pacmanSize
