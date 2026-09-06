{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Node where
import Graphics.UI.WX
    (   set, Prop(..) , penColor, penKind, penWidth
        , brushColor, brushKind, DC, point
    )
import Graphics.UI.WXCore
    (   dcDrawLine, dcDrawCircle, Point, Point2(..)  
        , PenKind(..), BrushKind(..)
    )
import Data.Array               ( Array, array, bounds, range, (!) )
import Data.List                ( sortOn )
import Data.Hashable            ( Hashable, hashWithSalt )
import qualified Data.HashMap.Strict as HM

import Paths                    ( getDataFile )
import Helper
import qualified Constants as C



data Node = Node
    {   getValue    :: Point
        , getU      :: Maybe Point
        , getD      :: Maybe Point
        , getL      :: Maybe Point
        , getR      :: Maybe Point
        , getPortal :: Maybe Point
    }

instance Show Node where
    show (Node v u d l r p) =
        "Node at " ++ show v ++ " {"
          ++ showMaybe u ++ ", "
          ++ showMaybe d ++ ", "
          ++ showMaybe l ++ ", "
          ++ showMaybe r ++ ", "
          ++ showMaybe p ++ "}"
        where
            showMaybe = maybe "Nothing" show



newtype NodeGroup = NodeGroup 
    { 
        fromNodeGroup :: [Node]
    }

instance Show NodeGroup where
    show (NodeGroup ns) = "NodeGroup [ " ++ unwords (map show ns) ++ " ]"



instance (Hashable a, Num a) => Hashable (Point2 a) where
    hashWithSalt salt (Point x y) =
        salt `hashWithSalt` x `hashWithSalt` y



getMaybeNeighborFromDirection :: Node -> String -> Maybe Point
getMaybeNeighborFromDirection node direction =
    case direction of
        "u" -> getU node
        "d" -> getD node
        "l" -> getL node
        "r" -> getR node
        _   -> Nothing

-- we assume the keys in the hashmap are the same as the values of the nodes
getNodesMap :: IO (HM.HashMap Point Node)
getNodesMap = do
    arr  <- loadMazeArray $ getDataFile "maze1.txt"
    return $ makeNodesMap arr

loadMazeArray :: FilePath -> IO (Array (Int, Int) Char)
loadMazeArray path = do
    text   <- readFile path
    let rows     = map (filter (/=' ')) (lines text)
        nRows    = length rows
        nCols    = length (head rows)
        indices  = [ (row,column)                           | row <- [0..(nRows-1)], column <- [0..(nCols-1)] ]
        elements = [ ((row,column), rows !! row !! column)  | (row,column) <- indices ]
    return $ array ((0,0), (nRows-1, nCols-1)) elements

getNodeGroupFromMap :: HM.HashMap Point Node -> NodeGroup
getNodeGroupFromMap hashMap =
    let orderedKeys = sortOn (\(Point x y) -> (y,x)) (HM.keys hashMap)
        nodes       = map (hashMap HM.!) orderedKeys
    in  NodeGroup nodes

getSafeStartKeyFromNodes :: HM.HashMap Point Node -> Point -> Point
getSafeStartKeyFromNodes hashMap startKey =
    let fallback = getValue (head (HM.elems hashMap))
    in 
        if HM.member startKey hashMap
            then startKey
            else fallback

getNodeFromPixels :: HM.HashMap Point Node -> Point -> Node
getNodeFromPixels hashMap p = hashMap HM.! p

getNodeFromTiles :: HM.HashMap Point Node -> (Int, Int) -> Node
getNodeFromTiles hashMap (column, row) =
    let key = constructKey column row
    in  hashMap HM.! key

isNodeSymbol :: Char -> Bool
isNodeSymbol c = elem c C.mazeNodeSymbols

isPathSymbol :: Char -> Bool
isPathSymbol c = elem c C.mazePathSymbols

lookupNode :: Point -> HM.HashMap Point Node -> Maybe Node
lookupNode p hashMap = HM.lookup p hashMap

lookupXYNode :: (Int, Int) -> HM.HashMap Point Node -> Maybe Node
lookupXYNode (x, y) hashMap = HM.lookup (point x y) hashMap

makeNodesMap :: Array (Int,Int) Char -> HM.HashMap Point Node
makeNodesMap arr =
    let baseList :: [ (Point, Node) ]
        baseList = 
            [ 
                let p = constructKey column row
                in  (p, mkEmptyNode p)
                | (row, column) <- range (bounds arr)
                , isNodeSymbol $ arr ! (row, column)
            ]

        baseHashMap = HM.fromList baseList
        hashMap1 = connectHorizontally arr baseHashMap
        hashMap2 = connectVertically   arr hashMap1
        hashMap3 = setPortalPair C.portalTile1 C.portalTile2 hashMap2
    in
        hashMap3

mkEmptyNode :: Point -> Node
mkEmptyNode p = Node 
    {   getValue    = p
        , getU      = Nothing
        , getD      = Nothing
        , getL      = Nothing
        , getR      = Nothing
        , getPortal = Nothing
    }

-- | Connect every node→right and right→node in each row
connectHorizontally :: Array (Int,Int) Char -> HM.HashMap Point Node -> HM.HashMap Point Node
connectHorizontally gridArr initialHashMap =
    let ((rowLow, columnLow), (rowHigh, columnHigh)) = bounds gridArr
        rowList    = [rowLow    .. rowHigh]
        columnList = [columnLow .. columnHigh]
    in connectIn gridArr initialHashMap rowList columnList
           (\node newKey      -> node { getR = Just newKey })
           (\node previousKey -> node { getL = Just previousKey })

-- | Connect every node→down and down→node in each column
connectVertically :: Array (Int,Int) Char -> HM.HashMap Point Node -> HM.HashMap Point Node
connectVertically gridArr initialHashMap =
    let ((rowLow, columnLow), (rowHigh, columnHigh)) = bounds gridArr
        rowList    = [rowLow    .. rowHigh]
        columnList = [columnLow .. columnHigh]
    in connectIn gridArr initialHashMap columnList rowList
           (\node newKey      -> node { getD = Just newKey })
           (\node previousKey -> node { getU = Just previousKey })

-- | A generic “connect along a grid” function.
--   We walk each index in ‘majorList’, then each in ‘minorList’,
--   threading the HashMap and an optional last‐seen node key.
connectIn
    :: Array (Int,Int) Char
    -> HM.HashMap Point Node
    -> [Int]
    -> [Int]
    -> (Node -> Point -> Node)    -- set forward link on previous node
    -> (Node -> Point -> Node)    -- set backward link on current node
    -> HM.HashMap Point Node
connectIn gridArr initialHashMap majorList minorList setForward setBackward =
    foldl connectOneMajor initialHashMap majorList
    where
        ((rowLow, _), (rowHigh, _)) = bounds gridArr

        connectOneMajor :: HM.HashMap Point Node -> Int -> HM.HashMap Point Node
        connectOneMajor hashMapAcc majorIndex =
            fst $ foldl (stepThroughMinor majorIndex) (hashMapAcc, Nothing) minorList

        stepThroughMinor :: Int -> (HM.HashMap Point Node, Maybe Point) -> Int -> (HM.HashMap Point Node, Maybe Point)
        stepThroughMinor majorIndex (hashMapAcc, mPreviousKey) minorIndex =
            let currentChar = gridArr !
                                ( if majorList == [rowLow .. rowHigh]
                                    then (majorIndex, minorIndex)
                                    else (minorIndex, majorIndex)
                                )
                currentKey  = constructKey
                                ( if majorList == [rowLow .. rowHigh]
                                    then minorIndex 
                                    else majorIndex
                                )
                                ( if majorList == [rowLow .. rowHigh]
                                    then majorIndex
                                    else minorIndex
                                )
            in case () of
                _ | isNodeSymbol currentChar ->
                    case mPreviousKey of
                        Nothing ->
                            (hashMapAcc, Just currentKey)
                        Just previousKey ->
                            let afterForward  =
                                    HM.adjust (\n -> setForward  n currentKey)  previousKey hashMapAcc
                                afterBackward =
                                    HM.adjust (\n -> setBackward n previousKey) currentKey  afterForward
                            in (afterBackward, Just currentKey)
                  | isPathSymbol currentChar ->
                    (hashMapAcc, mPreviousKey)
                  | otherwise ->
                    (hashMapAcc, Nothing)

constructKey :: Int -> Int -> Point
constructKey column row = point (column * C.tileWidth) (row * C.tileHeight)

-- HashMap.adjust :: (v -> v) -> k -> HashMap k v -> HashMap k v
-- HashMap.adjust f key m = ...
-- adjust applies the value transformation function f to the entry with given key. 
-- If no entry for that key exists then the map is left unchanged.
setPortalPair :: (Int, Int) -> (Int, Int) -> HM.HashMap Point Node -> HM.HashMap Point Node
setPortalPair (column1, row1) (column2, row2) hashMap0 =
    let key1    = constructKey column1 row1
        key2    = constructKey column2 row2
        mNode1  = HM.lookup key1 hashMap0 :: Maybe Node
        mNode2  = HM.lookup key2 hashMap0 :: Maybe Node
    in
        case (mNode1, mNode2) of
            (Just _, Just _) -> 
                let
                    hashMap1 = HM.adjust (\node -> node { getPortal = Just key2 } ) key1 hashMap0
                    hashMap2 = HM.adjust (\node -> node { getPortal = Just key1 } ) key2 hashMap1
                in
                    hashMap2
            _ ->
                hashMap0



drawNodes :: NodeGroup -> DC a -> IO ()
drawNodes nodeGroup dc = do
    drawNeighbors nodeGroup dc
    drawRedPoints nodeGroup dc

drawPortalNodes :: NodeGroup -> DC a -> IO ()
drawPortalNodes nodes dc = do
    let portalNodes = filter (\node -> (getPortal node) /= Nothing) (fromNodeGroup nodes)
    drawPinkPoint (getValue $ portalNodes !! 0) dc
    drawPinkPoint (getValue $ portalNodes !! 1) dc

drawNeighbors :: NodeGroup -> DC a -> IO ()
drawNeighbors (NodeGroup []) _       = return ()
drawNeighbors (NodeGroup (n:ns)) dc  = do
    let p = getValue n
    drawMaybeNeighbor p (getU n) dc
    drawMaybeNeighbor p (getD n) dc
    drawMaybeNeighbor p (getL n) dc
    drawMaybeNeighbor p (getR n) dc

    drawNeighbors (NodeGroup ns) dc

drawMaybeNeighbor :: Point -> Maybe Point -> DC a -> IO ()
drawMaybeNeighbor _ Nothing  _    = return ()
drawMaybeNeighbor point1 (Just point2) dc = drawWhiteLine point1 point2 dc

drawWhiteLine :: Point -> Point -> DC a -> IO ()
drawWhiteLine point1 point2 dc = do
    set dc [ penColor := rgb3 C.white
           , penKind  := PenSolid
           , penWidth := 2
           ]
    dcDrawLine   dc point1 point2

drawRedPoints :: NodeGroup -> DC a -> IO ()
drawRedPoints (NodeGroup []) _       = return ()
drawRedPoints (NodeGroup (n:ns)) dc  = do
    let p = getValue n
    drawRedPoint p dc

    drawRedPoints (NodeGroup ns) dc

drawRedPoint :: Point -> DC a -> IO ()
drawRedPoint p dc = do
    set dc [ brushColor := rgb3 C.red
           , brushKind  := BrushSolid
           , penColor := rgb3 C.red
           , penKind  := PenSolid
           , penWidth := 2
           ]
    let radius = 5
    dcDrawCircle dc p radius

drawPinkPoint :: Point -> DC a -> IO ()
drawPinkPoint p dc = do
    set dc [ brushColor := rgb3 C.pink
           , brushKind  := BrushSolid
           , penColor := rgb3 C.pink
           , penKind  := PenSolid
           , penWidth := 2
           ]
    let radius = 5
    dcDrawCircle dc p radius
