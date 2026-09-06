module Entity where
import Graphics.UI.WX as WX hiding ( key )
import qualified Data.HashMap.Strict as HM

import Node
import Vec



getCurrentNodeUpdate :: Point -> HM.HashMap Point Node -> Point
getCurrentNodeUpdate targetNodeKey hashMap =
    case (HM.lookup targetNodeKey hashMap) of
        Nothing         -> targetNodeKey
        Just targetNode -> case (getPortal targetNode) of
            Nothing                 -> targetNodeKey
            Just portalNeighborKey  -> portalNeighborKey

isTargetOvershot :: Vec Float -> Vec Float -> Vec Float -> Bool
isTargetOvershot currentPos currentNodePos targetNodePos = 
    let
        nodeToTargetDistance     = manhattenDistance targetNodePos currentNodePos
        nodeToCurrentPosDistance = manhattenDistance currentPos currentNodePos
    in
        nodeToCurrentPosDistance >= nodeToTargetDistance
