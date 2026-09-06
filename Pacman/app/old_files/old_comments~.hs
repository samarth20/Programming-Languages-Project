    --                     -- bundle all four into one behavior of a 4‐tuple
                -- bAll :: Behavior (Point, Node, Node, String)
                -- bAll = liftA4 (,,,) bPosFloat bCurrentNode bTargetNode bCurrentDir

            -- on every tick, pull out the latest 4‐tuple and print it all at once:
            -- print all the information about bState
            -- reactimate $ (\(pos,cur,tgt,dir) ->
            --     putStrLn $  "pos = " ++ show pos
            --             ++ ", current = " ++ show cur
            --             ++ ", target = " ++ show tgt
            --             ++ ", dir = " ++ show dir) <$> bState <@ etick
    --     -- TODO: delete
    --     portalNodes = filter (\node -> (getPortal node) /= Nothing) (fromNodeGroup nodes)
    --     currentNodeUpdate = getCurrentNodeUpdate (getValue $ portalNodes !! 1) nodesMap
    -- print portalNodes
    -- print (portalNodes !! 1)
    -- print currentNodeUpdate

-- updatePacmanState :: String -> (Vec Float, Node, Node, String) -> (Vec Float, Node, Node, String)
-- updatePacmanState userDirection (currentPosition, currentNode, targetNode, currentDirection) =
--     if isOppositeDirection userDirection currentDirection
--         then 
--             let 
--                 newPosition = updatePacmanPosition currentPosition userDirection
--                 currentNode' = targetNode
--                 targetNode' = currentNode
--                 currentDirection' = userDirection
--             in
--                 (   newPosition
--                     , currentNode'
--                     , targetNode'
--                     , currentDirection'
--                 )
--         else
--             let
--                 newPosition = updatePacmanPosition currentPosition currentDirection
--             in
--                 if isTargetOvershot newPosition (getVecValue currentNode) (getVecValue targetNode)
--                     then
--                         let
--                             currentNode' = targetNode
--                             newPosition' = getVecValue currentNode'
--                             (currentDirection', targetNode') = getNewTarget userDirection currentDirection targetNode
--                         in
--                             (   newPosition'
--                                 , currentNode'
--                                 , targetNode' 
--                                 , currentDirection'
--                             )
--                     else
--                         (   newPosition
--                             , currentNode
--                             , targetNode 
--                             , currentDirection
--                         )
-- -- link left↔right.
-- connectHorizontally :: Array (Int,Int) Char -> HM.HashMap Point Node -> HM.HashMap Point Node
-- connectHorizontally arr hashMap0 = foldl linkRow hashMap0 [rowLow..rowHigh]
--     where
--         ((rowLow, columnLow), (rowHigh, columnHigh)) = bounds arr
--         linkRow hashMap row = fst $
--             foldl step (hashMap, Nothing) [columnLow..columnHigh]
--             where
--                 step (hashMapAcc, mPreviousKey) column =
--                     let char       = arr ! (row, column)
--                         currentKey = constructKey column row
--                     in case () of
--                         _ | isNodeSymbol char ->
--                             case mPreviousKey of
--                                 Nothing          -> (hashMapAcc, Just currentKey)
--                                 Just previousKey ->
--                                     let
--                                         hashMap1 = HM.adjust (\n -> n { getR = Just currentKey })  previousKey hashMapAcc
--                                         hashMap2 = HM.adjust (\n -> n { getL = Just previousKey }) currentKey  hashMap1
--                                     in  (hashMap2, Just currentKey)
--                           | isPathSymbol char ->
--                             (hashMapAcc, mPreviousKey)
--                           | otherwise         ->
--                             (hashMapAcc, Nothing)
  
-- --   link up↔down.
-- connectVertically :: Array (Int,Int) Char -> HM.HashMap Point Node -> HM.HashMap Point Node
-- connectVertically arr hashMap0 = foldl linkColumn hashMap0 [columnLow..columnHigh]
--     where
--         ((rowLow, columnLow), (rowHigh, columnHigh)) = bounds arr
--         linkColumn hashMap column = fst $
--             foldl step (hashMap, Nothing) [rowLow..rowHigh]
--             where
--                 step (hashMapAcc, mPreviousKey) row =
--                     let char       = arr ! (row, column)
--                         currentKey = constructKey column row
--                     in case () of
--                         _ | isNodeSymbol char ->
--                             case mPreviousKey of
--                                 Nothing          -> (hashMapAcc, Just currentKey)
--                                 Just previousKey ->
--                                     let
--                                         hashMap1 = HM.adjust (\n -> n { getD = Just currentKey })  previousKey hashMapAcc
--                                         hashMap2 = HM.adjust (\n -> n { getU = Just previousKey }) currentKey  hashMap1
--                                     in  (hashMap2, Just currentKey)
--                           | isPathSymbol char ->
--                             (hashMapAcc, mPreviousKey)
--                           | otherwise         ->
--                             (hashMapAcc, Nothing)

-- constructKey :: Int -> Int -> Point
-- constructKey c r = point (c * C.tileWidth) (r * C.tileHeight)
-- getStartNodeKey :: Point
-- getStartNodeKey = case lookupNode (point 16 16) nodesMap of
--                          Just _  -> point 16 16
--                          Nothing -> getValue (head (fromNodeGroup nodes))

-- startNode :: IO Node
-- startNode = head (fromNodeGroup getNodes)
-- startNode = (fromNodeGroup $ getNodes)!!0


-- startPosition :: Vec Float
-- startPosition = getVecValue startNode
    -- nodesMap <- getNodesMap    -- :: HashMap Point Node
    -- nodes2   <- getNodes2
        -- print $ (fromNodeGroup nodes) !! 0
    -- print $ (fromNodeGroup nodes) !! 1
    -- print $ (fromNodeGroup nodes) !! 2
    -- print $ (fromNodeGroup nodes) !! 3
    -- print $ (fromNodeGroup nodes) !! 4
    -- print $ (fromNodeGroup nodes) !! 5
    -- print $ (fromNodeGroup nodes) !! 6
    -- print $ (fromNodeGroup nodes2) !! 0
    -- print $ (fromNodeGroup nodes2) !! 1
    -- print $ (fromNodeGroup nodes2) !! 2
    -- print $ (fromNodeGroup nodes2) !! 3
    -- print $ (fromNodeGroup nodes2) !! 4
    -- print $ (fromNodeGroup nodes2) !! 5
    -- print $ (fromNodeGroup nodes2) !! 6

    -- print $ nodes
    -- print $ lookupNode (point 16 16)  nodesMap   -- Just n1 (if present)
    -- print $ lookupNode (point 16 48)  nodesMap   -- Just n1 (if present)
    -- print $ lookupNode (point 64 16)  nodesMap   -- Just n1 (if present)
    -- print $ lookupNode (point 64 48)  nodesMap
    -- print $ lookupNode (point 16 96)  nodesMap   -- Just n1 (if present)
    -- print $ lookupNode (point 96 96)  nodesMap
    -- print $ lookupNode (point 96 48)  nodesMap
    -- mazeArr <- loadMazeArray $ getDataFile "mazetest.txt"
    -- print $ mazeArr ! (2, 3)   -- 1-based indexing: row=2,col=3
    -- print $ (isNodeSymbol (mazeArr ! (2, 3)))
-- getPointString :: Point -> String
-- getPointString (Point x y) = "Point: " ++ (show x) ++ ", " ++ (show y)

-- (
--     (bPosFloat :: Behavior (Float, Float)),
--     (bCurrentNode :: Behavior Node),
--     (bTargetNode :: Behavior Node),
--     (bCurrentDirection :: Behavior String)
-- )
--     <- accumB ((100.0, 200.0), startNode, startNode, "s") $ updatePacmanPosition <$> bUserDir <@ etick
-- (bCurrentNode :: Behavior Node) <-


    -- set pMain [   on (charKey '-') := set t [interval :~ \i -> i * 2]
    --             , on (charKey 'z') := set t [interval :~ \i -> max 10 (div i 2)] -- todo: change back '+'
    --        ] --
            -- bpaint <- stepper (\_dc _ -> return ()) $
            --             (drawGameState <$> bship <*> brocks) <@ etick
            -- bpaint <- -- TODO: draw a yellow circle at (200, 400) as the starting position.
            --           --       If a key is pressed ("u", "d", "l" or "r"), the yellow circle moves 
            --           --       in the area beween (0, 0) and (C.screenWidth, C.mainPanelHeight)
            --           --       into the correct direction until a different key is pressed or the
            --           --       border of the area is reached.
            -- bMove <- stepper "s" $ eLeft
            -- $ unions
            --     [         <$ eCoin
            --     , subtract <$> bPrice <@ eBananaSold
            --     ]
            -- ship position
            -- (bship :: Behavior Int)
            --     <- accumB (C.screenwidth `div` 2) $ unions
            --         [ goLeft  <$ eleft
            --         , goRight <$ eright
            --         ]
            -- let
            --     goLeft  x = max 0          (x - 5)
            --     goRight x = min (C.screenwidth-30) (x + 5)

            -- -- rocks
            -- brandom <- fromPoll (randomRIO (0,1) :: IO Double)

            -- (brocks :: Behavior [Rock])
            --     <- accumB [] $ unions
            --         [ advanceRocks <$ etick
            --         , newRock      <$> filterE (< chance) (brandom <@ etick)
            --         ]

            -- -- draw the game state
            -- bpaint <- stepper (\_dc _ -> return ()) $
            --             (drawGameState <$> bship <*> brocks) <@ etick
            -- sink pMain [on paint :== bpaint]
            -- reactimate $ repaint pMain <$ etick

            -- -- status bar
            -- let bstatus :: Behavior String
            --     bstatus = (\r -> "rocks: " ++ show (length r)) <$> brocks
            -- sink status [text :== bstatus]

-- drawGameState :: IO ()
-- return ""


-- rock logic
-- type Position = Point2 Int
-- type Rock     = [Position] -- lazy list of future y-positions

-- newRock :: Double -> [Rock] -> [Rock]
-- newRock r rs = (track . floor $ fromIntegral C.screenwidth * r / chance) : rs

-- track :: Int -> Rock
-- track x = [point x (y - diameter) | y <- [0, 6 .. C.screenheight + 2 * diameter]]

-- advanceRocks :: [Rock] -> [Rock]
-- advanceRocks = filter (not . null) . map (drop 1)



-- draw game state
-- drawGameState :: Int -> [Rock] -> DC a -> b -> IO ()
-- drawGameState ship rocks dc _view = do
--     let
--         shipLocation = point ship (C.screenheight - 2 * diameter)
--         positions    = map head rocks
--         collisions   = map (collide shipLocation) positions

--     drawShip dc shipLocation
--     mapM (drawRock dc) (zip positions collisions)

--     when (or collisions) (play explode)

-- collide :: Position -> Position -> Bool
-- collide pos0 pos1 =
--     let distance = vecLength (vecBetween pos0 pos1)
--     in distance <= fromIntegral diameter

-- drawShip :: DC a -> Point -> IO ()
-- drawShip dc pos = drawBitmap dc ship pos True []

-- drawRock :: DC a -> (Point, Bool) -> IO ()
-- drawRock dc (pos, collides) =
--     let rockPicture = if collides then burning else rock
--     in drawBitmap dc rockPicture pos True []
