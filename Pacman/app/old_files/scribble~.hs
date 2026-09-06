{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

-- connect horizontally:
{-# LANGUAGE FlexibleContexts #-}
module Node where

import           Data.Array
import qualified Data.HashMap.Strict as HM
import           Graphics.UI.WXCore (Point2, point)
import qualified Constants as C

-- assume these are already in scope:
--   isNodeSymbol :: Char -> Bool
--   isPathSymbol :: Char -> Bool
--   constructKey :: Int -> Int -> Point2 Int
--   mkEmptyNode  :: Point2 Int -> Node
--   lookupNode   :: Point2 Int -> HM.HashMap (Point2 Int) Node -> Maybe Node

-- | For each row:
--   • whenever you see a node‐symbol, if there's a previous node in this row,
--     connect them (prev → right, curr → left),
--   • if you see a “break” symbol (not a path), reset the “previous node” to Nothing.
-- connectHorizontally
--   :: Array (Int,Int) Char
--   -> HM.HashMap (Point2 Int) Node
--   -> HM.HashMap (Point2 Int) Node
-- connectHorizontally arr initialLUT =
--     foldl connectOneRow initialLUT [rLo .. rHi]
--   where
--     ((rLo,cLo),(rHi,cHi)) = bounds arr

--     -- process a single row, carrying along the updated LUT and
--     -- the last‐seen node‐key in that row (if any)
--     connectOneRow
--       :: HM.HashMap (Point2 Int) Node
--       -> Int
--       -> HM.HashMap (Point2 Int) Node
--     connectOneRow lut0 row =
--       fst $ foldl (connectInCell row) (lut0, Nothing) [cLo .. cHi]

--     connectInCell
--       :: Int
--       -> ( HM.HashMap (Point2 Int) Node  -- current LUT
--          , Maybe (Point2 Int)            -- last node‐key in this row
--          )
--       -> Int
--       -> ( HM.HashMap (Point2 Int) Node
--          , Maybe (Point2 Int)
--          )
--     connectInCell row (lut, mPrevKey) col =
--       case arr ! (row,col) of

--         -- a node‐symbol: make or extend a horizontal chain
--         c | isNodeSymbol c ->
--           let key = constructKey col row
--           in case mPrevKey of

--             -- first node in a run: just remember it
--             Nothing ->
--               (lut, Just key)

--             -- second or later node in run: link to the previous,
--             Just prevKey ->
--               case (HM.lookup prevKey lut, HM.lookup key lut) of
--                 (Just node1, Just node2) ->
--                   -- update prevKey.right and key.left in the map
--                   let lut'  = HM.adjust (\n -> n { getR = Just node2 }) prevKey lut
--                       lut'' = HM.adjust (\n -> n { getL = Just node1 })  key    lut'
--                   in (lut'', Just key)
--                 _ -> (lut, Just key)

--         -- a path‐symbol: continue chain (don’t reset mPrevKey)
--         c | isPathSymbol c ->
--           (lut, mPrevKey)

--         -- anything else (wall/obstacle): break the chain
--         _ ->
--           (lut, Nothing)

module Node where
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
  show (NodeGroup ns) =
    "NodeGroup [ " ++ unwords (map (show . getValue) ns) ++ " ]"


getStringFromMaybeNode :: Maybe Node -> String
getStringFromMaybeNode Nothing  = "Nothing"
getStringFromMaybeNode (Just n) = show (getValue n)



getNodesLUT :: IO (HM.HashMap Point Node)
getNodesLUT = do
    arr  <- loadMazeArray $ getDataFile "mazetest.txt"
    return $ makeNodesLUT arr

startNode :: Node
startNode = (fromNodeGroup $ getNodes)!!0

startPosition :: Vec Float
startPosition = getVecValue startNode

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
    return $ array ((1,1), (nrows, ncols)) elems


lookupNode :: Point -> HM.HashMap Point Node -> Maybe Node
lookupNode p lut = HM.lookup p lut

lookupXYNode :: (Int, Int) -> HM.HashMap Point Node -> Maybe Node
lookupXYNode (x, y) lut = HM.lookup (point x y) lut


makeNodesLUT :: Array (Int, Int) Char -> HM.HashMap Point Node
makeNodesLUT arr =
    let
        coordsAndNodes = [ 
                (p, mkEmptyNode p)
                    |   (i,j) <- range (bounds arr)
                        , isNodeSymbol (arr ! (i,j))
                        , let p = (constructKey i j)
            ]
        hm = HM.fromList coordsAndNodes
        connectedHm = connectHorizontally arr hm
    in
        connectedHm


connectHorizontally :: Array (Int, Int) Char -> HM.HashMap Point Node -> HM.HashMap Point Node
connectHorizontally arr initialLUT =
    foldl connectOneRow initialLUT [rLo .. rHi]
  where
    ((rLo,cLo),(rHi,cHi)) = bounds arr

    -- process a single row, carrying along the updated LUT and
    -- the last‐seen node‐key in that row (if any)
    connectOneRow
      :: HM.HashMap (Point2 Int) Node
      -> Int
      -> HM.HashMap (Point2 Int) Node
    connectOneRow lut0 row =
      fst $ foldl (connectInCell row) (lut0, Nothing) [cLo .. cHi]

    connectInCell
      :: Int
      -> ( HM.HashMap (Point2 Int) Node  -- current LUT
         , Maybe (Point2 Int)            -- last node‐key in this row
         )
      -> Int
      -> ( HM.HashMap (Point2 Int) Node
         , Maybe (Point2 Int)
         )
    connectInCell row (lut, mPrevKey) col =
      case arr ! (row,col) of

        -- a node‐symbol: make or extend a horizontal chain
        c | isNodeSymbol c ->
          let key = constructKey col row
          in case mPrevKey of

            -- first node in a run: just remember it
            Nothing ->
              (lut, Just key)

            -- second or later node in run: link to the previous,
            Just prevKey ->
              case (HM.lookup prevKey lut, HM.lookup key lut) of
                (Just node1, Just node2) ->
                  -- update prevKey.right and key.left in the map
                  let lut'  = HM.adjust (\n -> n { getR = Just node2 }) prevKey lut
                      lut'' = HM.adjust (\n -> n { getL = Just node1 })  key    lut'
                  in (lut'', Just key)
                _ -> (lut, Just key)

        -- a path‐symbol: continue chain (don’t reset mPrevKey)
        c | isPathSymbol c ->
          (lut, mPrevKey)

        -- anything else (wall/obstacle): break the chain
        _ ->
          (lut, Nothing)


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

getNodes :: NodeGroup
getNodes = NodeGroup (HM.elems getNodesLUT)

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
