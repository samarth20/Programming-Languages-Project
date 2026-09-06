{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}


module Node_temp where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Array
import System.IO
import qualified Data.HashMap.Strict as HM
import GHC.Generics  (Generic)
import Data.Hashable (Hashable, hashWithSalt)

import Vec
import qualified Constants as C
import Paths (getDataFile)



instance (Hashable a, Num a) => Hashable (Point2 a) where
  hashWithSalt salt (Point x y) =
    salt `hashWithSalt` x `hashWithSalt` y



data Node = Node
    {   getValue        :: Point
        , getU          :: Maybe Node
        , getD          :: Maybe Node
        , getL          :: Maybe Node
        , getR          :: Maybe Node
    }


newtype NodeGroup = NodeGroup 
    { 
        fromNodeGroup :: [Node]
        -- nodeSymbols :: [Char] -- TODO: delete?
    }


instance Show Node where
    show (Node p mnU mnD mnL mnR) = 
        let
            mnUString = getStringFromMaybeNode mnU
            mnDString = getStringFromMaybeNode mnD
            mnLString = getStringFromMaybeNode mnL
            mnRString = getStringFromMaybeNode mnR
        in
            "Node at " 
            ++ show p ++ " {" 
            ++ mnUString ++ ", " 
            ++ mnDString ++ ", " 
            ++ mnLString ++ ", " 
            ++ mnRString ++ "}" 


instance Show NodeGroup where
  show (NodeGroup ns) = "NodeGroup [ " ++ unwords (map show ns) ++ " ]"


getStringFromMaybeNode :: Maybe Node -> String
getStringFromMaybeNode Nothing  = "Nothing"
getStringFromMaybeNode (Just n) = show (getValue n)



getNodesMap :: IO (HM.HashMap Point Node)
getNodesMap = do
    arr  <- loadMazeArray $ getDataFile "mazetest.txt"
    return $ makeNodesMap arr
-- TODO: delete
-- getNodesMap :: HM.HashMap (Int,Int) Node
-- getNodesMap =
--     let 
--         maze = loadMazeArray $ getDataFile "mazetest.txt"
--     in
--         makeNodesMap maze

-- TODO: move this somewhere
getNodes :: IO NodeGroup
getNodes = do
    hm <- getNodesMap            -- :: HashMap Point Node
    let nodes = HM.elems hm      -- :: [Node]
    return (NodeGroup nodes)
-- getNodes :: NodeGroup
-- getNodes = NodeGroup (HM.elems getNodesMap)

-- startNode :: IO Node
-- startNode = head (fromNodeGroup getNodes)
-- startNode = (fromNodeGroup $ getNodes)!!0


-- startPosition :: Vec Float
-- startPosition = getVecValue startNode
-- end TODO

getNodeSymbols :: [Char]
getNodeSymbols = ['+']

getPathSymbols :: [Char]
getPathSymbols = ['.']

isNodeSymbol :: Char -> Bool
isNodeSymbol c = elem c getNodeSymbols

isPathSymbol :: Char -> Bool
isPathSymbol c = elem c getPathSymbols

loadMazeArray :: FilePath -> IO (Array (Int, Int) Char)
loadMazeArray path = do
    txt   <- readFile path
    let rows    = map (filter (/=' ')) (lines txt)
        nrows   = length rows
        ncols   = length (head rows)
        idxs    = [ (r,c) | r <- [0..(nrows-1)], c <- [0..(ncols-1)] ]
        elems   = [ ((r,c), rows !! r !! c) | (r,c) <- idxs ]
    return $ array ((0,0), (nrows-1, ncols-1)) elems


lookupNode :: Point -> HM.HashMap Point Node -> Maybe Node
lookupNode p hm = HM.lookup p hm

lookupXYNode :: (Int, Int) -> HM.HashMap Point Node -> Maybe Node
lookupXYNode (x, y) hm = HM.lookup (point x y) hm


makeNodesMap :: Array (Int, Int) Char -> HM.HashMap Point Node
makeNodesMap arr =
   let
        coordsAndNodes =
                [ let p = constructKey col row
                in (p, mkEmptyNode p)
                | (row,col) <- range (bounds arr)
                , isNodeSymbol (arr ! (row,col))
                ]
        baseHM = HM.fromList coordsAndNodes
        h1     = connectHorizontally arr baseHM
        h2     = connectVertically   arr h1
    in
        h2
      


connectHorizontally :: Array (Int, Int) Char -> HM.HashMap Point Node -> HM.HashMap Point Node
connectHorizontally arr initialHashMap =
    foldl connectOneRow initialHashMap [rLo .. rHi]
  where
    ((rLo,cLo),(rHi,cHi)) = bounds arr
    -- process a single row, carrying along the updated HashMap and
    -- the last‐seen node‐key in that row (if any)
    connectOneRow :: HM.HashMap Point Node -> Int -> HM.HashMap Point Node
    connectOneRow hm0 row = fst $ foldl (connectInCell row) (hm0, Nothing) [cLo .. cHi]
    
    connectInCell ::         Int 
                        --  (current HashMap, last node‐key in this row)
                        ->  ( HM.HashMap (Point2 Int) Node, Maybe (Point2 Int))
                        ->  Int
                        ->  ( HM.HashMap (Point2 Int) Node, Maybe (Point2 Int))
    connectInCell row (hm, mPrevKey) col =
        case arr ! (row,col) of
        
        -- a node‐symbol: make or extend a horizontal chain
        c | isNodeSymbol c ->
                let key = constructKey col row
                in case mPrevKey of
                    Nothing -> (hm, Just key)
                    (Just prevKey) -> case (HM.lookup prevKey hm, HM.lookup key hm) of
                        (Just node1, Just node2) ->
                                -- update prevKey.right and key.left in the map
                            let (hm', jKey) = updateNodePair prevKey key hm
                            in (hm', jKey)
                        _ -> (hm, Just key)
        
        -- a path‐symbol: continue chain (don’t reset mPrevKey)
        c | isPathSymbol c -> (hm, mPrevKey)
        
        -- anything else (wall/obstacle): break the chain
        _ -> (hm, Nothing)

updateNodePair :: Point -> Point -> HM.HashMap Point Node -> (HM.HashMap Point Node, Maybe Point)
updateNodePair leftKey rightKey hm =
    let mLeftNode        = HM.lookup leftKey hm
        mRightNode       = HM.lookup rightKey hm
    in case (mLeftNode, mRightNode) of
        (Just leftNode, Just rightNode) -> 
            let leftNode'  = leftNode { getR=Just rightNode' }
                rightNode' = rightNode { getL=Just leftNode' }
                hm'             = HM.insert leftKey leftNode' hm
                hm''            = HM.insert rightKey rightNode' hm'
            in (hm'', Just rightKey)
        (_, _) -> (hm, Just rightKey)

-- | Stitches vertically-adjacent nodes: whenever two '+'-cells appear in the same
-- column with only path-symbols between them, link “above”.getD → “below”
-- and “below”.getU → “above”.
connectVertically
  :: Array (Int,Int) Char
  -> HM.HashMap Point Node
  -> HM.HashMap Point Node
connectVertically arr initialHM =
    foldl connectOneColumn initialHM [cLo .. cHi]
  where
    ((rLo,cLo),(rHi,cHi)) = bounds arr

    -- For one fixed column, walk down all rows
    connectOneColumn
      :: HM.HashMap Point Node
      -> Int                   -- ^ column index
      -> HM.HashMap Point Node
    connectOneColumn hm0 col =
      fst $ foldl (connectCell col) (hm0, Nothing) [rLo .. rHi]

    connectCell
      :: Int                   -- ^ fixed column
      -> (HM.HashMap Point Node, Maybe Point)
         -- ^ (current map, last-seen node-key in this column)
      -> Int                   -- ^ current row
      -> (HM.HashMap Point Node, Maybe Point)
    connectCell col (hm,mPrevKey) row =
      case arr ! (row,col) of

        -- saw a node: either remember it or link to the previous one
        c | isNodeSymbol c ->
          let key = constructKey col row
          in case mPrevKey of
               Nothing        -> (hm, Just key)
               Just prevKey  ->
                 case (HM.lookup prevKey hm, HM.lookup key hm) of
                   (Just n1, Just n2) ->
                     let (hm', _) = updateVerticalPair prevKey key hm
                     in (hm', Just key)
                   _ -> (hm, Just key)

        -- path: keep the same “previous node”
        c | isPathSymbol c ->
          (hm, mPrevKey)

        -- anything else: break the vertical chain
        _ ->
          (hm, Nothing)

-- | Link two nodes topKey → bottomKey vertically:
--   topNode.getD = Just bottomNode
--   bottomNode.getU = Just topNode
updateVerticalPair
  :: Point                    -- ^ top-node key
  -> Point                    -- ^ bottom-node key
  -> HM.HashMap Point Node
  -> (HM.HashMap Point Node, Maybe Point)
updateVerticalPair topKey botKey hm =
  case (HM.lookup topKey hm, HM.lookup botKey hm) of
    (Just topN, Just botN) ->
      let topN'  = topN { getD = Just botN' }
          botN'  = botN { getU = Just topN' }
          hm'    = HM.insert topKey topN' hm
          hm''   = HM.insert botKey botN' hm'
      in (hm'', Just botKey)
    _ -> (hm, Just botKey)


-- let hm2 :: HM.HashMap (Int,Int) Node
--     hm2 = foldl'
--       (\hm n ->
--          let Point x y = getValue n
--          in HM.insert (x,y) n hm
--       ) HM.empty nodes
-- connectHorizontally :: Array (Int, Int) Char -> HM.HashMap Point Node -> HM.HashMap Point Node
-- connectHorizontally arr hm =
--     let
--         connectedHorizontalNodes = getNodePairs arr
--     in
--         foldl (\hm (p1, p2) -> connect2HorizontalNodes p1 p2 hm) hm connectedHorizontalNodes

-- getNodePairs :: Array (Int, Int) Char -> [[(Int, Int)]]
-- getNodePairs arr =
--     let 
--         elems = [
--                 let 
--                     row = [arr!(i,jIdx) | jIdx <- [jLow..jHigh]]
--                 in
--                     getRowNodePairs row
--                         |   let ((iLow,jLow),(iHigh,jHigh)) = bounds arr
--                         , i <- [iLow..iHigh]
--             ]
--     in 
--         array (bounds arr) elems

-- getRowNodePairs :: [Char] -> [(Int, Int)]
-- getRowNodePairs row =
--     foldl (\(key, ans) a -> 
--         if elem a nodeSymbols
--             then (a, )) (Nothing, Nothing) row

-- connect2HorizontalNodes :: Point -> Point -> HM.HashMap Point Node -> HM.HashMap Point Node
-- connect2HorizontalNodes leftPoint rightPoint hm =
--     let leftNode        = HM.lookup leftPoint hm
--         rightNode       = HM.lookup rightPoint hm
--         leftNode'       = leftNode { getR=rightNode' }
--         rightNode'      = rightNode { getL=leftNode' }
--         hm'             = HM.insert leftPoint leftNode' hm
--         hm''            = HM.insert rightPoint rightNode' hm'
--     in
--         hm''
        

--     def connectHorizontally(self, data, xoffset=0, yoffset=0):
--         for row in list(range(data.shape[0])):
--             key = None
--             for col in list(range(data.shape[1])):
--                 if data[row][col] in self.nodeSymbols:
--                     if key is None:
--                         key = self.constructKey(col+xoffset, row+yoffset)
--                     else:
--                         otherkey = self.constructKey(col+xoffset, row+yoffset)
--                         self.NodesMap[key].neighbors[RIGHT] = self.NodesMap[otherkey]
--                         self.NodesMap[otherkey].neighbors[LEFT] = self.NodesMap[key]
--                         key = otherkey
--                 elif data[row][col] not in self.pathSymbols:
--                     key = None
-- makeNodesMap :: IO (Array (Int, Int) Char) -> HM.HashMap (Int, Int) Node
-- makeNodesMap arr = 
--     let ((iLo,jLo),(iHi,jHi)) = bounds arr
--         coordsAndNodes = 
--             [ 
--                 if isNodeSymbol $ arr ! (i,j)
--                     then ((i,j), Just (mkEmptyNode $ constructKey i j))
--                     else ((i,j), Nothing)
--                     | (i,j) <- range $ bounds arr 
--             ]
--     in
--         HM.fromList coordsAndNodes

constructKey :: Int -> Int -> Point2 Int
constructKey x y = point (x * C.tileWidth) (y * C.tileHeight)

p1 :: Point2 Int
p2 :: Point2 Int
p3 :: Point2 Int
p4 :: Point2 Int
p5 :: Point2 Int
p6 :: Point2 Int
p7 :: Point2 Int
p1 = point 80 80
p2 = point 160 80
p3 = point 80 160
p4 = point 160 160
p5 = point 208 160
p6 = point 80 320
p7 = point 208 320


n1 :: Node
n2 :: Node
n3 :: Node
n4 :: Node
n5 :: Node
n6 :: Node
n7 :: Node
n1 = mkNode p1 ( Nothing, Just n3, Nothing, Just n2 )
n2 = mkNode p2 ( Nothing, Just n4, Just n1, Nothing )
n3 = mkNode p3 ( Just n1, Just n6, Nothing, Just n4 )
n4 = mkNode p4 ( Just n2, Nothing, Just n3, Just n5 )
n5 = mkNode p5 ( Nothing, Just n7, Just n4, Nothing )
n6 = mkNode p6 ( Just n3, Nothing, Nothing, Just n7 )
n7 = mkNode p7 ( Just n5, Nothing, Just n6, Nothing )


mkNode :: Point -> ( Maybe Node, Maybe Node, Maybe Node, Maybe Node ) -> Node
mkNode p (u, d, l, r) = Node { getValue=p, getU=u, getD=d , getL=l , getR=r }

mkEmptyNode :: Point -> Node
mkEmptyNode p         = Node { getValue=p, getU=Nothing, getD=Nothing , getL=Nothing , getR=Nothing } 

getFloatPointNodeValue :: Node -> Point2 Float
getFloatPointNodeValue (Node (Point x y) _ _ _ _) = point (fromIntegral x) (fromIntegral y)


getVecValue :: Node -> Vec Float
getVecValue (Node (Point x y) _ _ _ _) = mkVecFloat (fromIntegral x) (fromIntegral y)


drawNodes :: NodeGroup -> DC a -> IO ()
drawNodes nodeGroup dc = do
    drawNeighbors nodeGroup dc
    drawRedPoints nodeGroup dc


drawNeighbors :: NodeGroup -> DC a -> IO ()
drawNeighbors (NodeGroup []) _       = return ()
drawNeighbors (NodeGroup (n:ns)) dc  = do
    let p = getValue n
    drawMaybeNeighbor p (getMaybeNodeValue $ getU n) dc
    drawMaybeNeighbor p (getMaybeNodeValue $ getD n) dc
    drawMaybeNeighbor p (getMaybeNodeValue $ getL n) dc
    drawMaybeNeighbor p (getMaybeNodeValue $ getR n) dc

    drawNeighbors (NodeGroup ns) dc


getMaybeNodeValue :: Maybe Node -> Maybe Point
getMaybeNodeValue Nothing  = Nothing
getMaybeNodeValue (Just n) = Just (getValue n)


drawMaybeNeighbor :: Point -> Maybe Point -> DC a -> IO ()
drawMaybeNeighbor _ Nothing  _    = return ()
drawMaybeNeighbor point1 (Just point2) dc = drawWhiteLine point1 point2 dc


drawWhiteLine :: Point -> Point -> DC a -> IO ()
drawWhiteLine point1 point2 dc = do
    set dc [ penColor := white
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
    set dc [ brushColor := red
           , brushKind  := BrushSolid
           , penColor := red
           , penKind  := PenSolid
           , penWidth := 2
           ]
    let radius = 5
    dcDrawCircle dc p radius


-- class NodeGroup(object):
--     def __init__(self, level):
--         self.level = level
--         self.NodesMap = {}
--         self.nodeSymbols = ['+']
--         self.pathSymbols = ['.']
--         data = self.readMazeFile(level)
--         self.createNodeTable(data)
-- 	self.connectHorizontally(data)
-- 	self.connectVertically(data)
--     def createNodeTable(self, data, xoffset=0, yoffset=0):
--         for row in list(range(data.shape[0])):
--             for col in list(range(data.shape[1])):
--                 if data[row][col] in self.nodeSymbols:
--                     x, y = self.constructKey(col+xoffset, row+yoffset)
--                     self.NodesMap[(x, y)] = Node(x, y)

--     def constructKey(self, x, y):
--         return x * TILEWIDTH, y * TILEHEIGHT
-- let hm2 :: HM.HashMap (Int,Int) Node
--     hm2 = foldl'
--       (\hm n ->
--          let Point x y = getValue n
--          in HM.insert (x,y) n hm
--       ) HM.empty nodes
-- makeNodesMap :: [Node] -> HM.HashMap (Int,Int) Node
-- makeNodesMap = HM.fromList 
--     . map (\n -> let Point x y = getValue n in ((x,y),n))

-- TODO: delete? This is just for testing
-- getNodes2 :: NodeGroup
-- getNodes2 = NodeGroup [n1, n2, n3, n4, n5, n6, n7]

mkNode :: Point -> ( Maybe Point, Maybe Point, Maybe Point, Maybe Point ) -> Node
mkNode p (u, d, l, r) = Node { getValue=p, getU=u, getD=d , getL=l , getR=r, getPortal=Nothing }

p1 :: Point
p2 :: Point
p3 :: Point
p4 :: Point
p5 :: Point
p6 :: Point
p7 :: Point
p1 = point 16 16
p2 = point 64 16 
p3 = point 16 48
p4 = point 64 48
p5 = point 96 48 
p6 = point 16 96
p7 = point 96 96

n1 :: Node
n2 :: Node
n3 :: Node
n4 :: Node
n5 :: Node
n6 :: Node
n7 :: Node
n1 = mkNode p1 ( Nothing, Just p3, Nothing, Just p2 )
n2 = mkNode p2 ( Nothing, Just p4, Just p1, Nothing )
n3 = mkNode p3 ( Just p1, Just p6, Nothing, Just p4 )
n4 = mkNode p4 ( Just p2, Nothing, Just p3, Just p5 )
n5 = mkNode p5 ( Nothing, Just p7, Just p4, Nothing )
n6 = mkNode p6 ( Just p3, Nothing, Nothing, Just p7 )
n7 = mkNode p7 ( Just p5, Nothing, Just p6, Nothing )
