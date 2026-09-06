{-|
Module      : Pellets
Description : A module representing a network of pellets

This module is implemented the same way as the Node module.
-}

module Pellets where
import Graphics.UI.WX
import Graphics.UI.WXCore



data Pellets = Pellets
    {   getValue        :: Point
        , getU          :: Maybe Pellets
        , getD          :: Maybe Pellets
        , getL          :: Maybe Pellets
        , getR          :: Maybe Pellets
    }


newtype PelletsGroup = PelletsGroup { fromPelletsGroup :: [Pellets] } 


instance Show Pellets where
    show (Pellets p mnU mnD mnL mnR) = 
        let
            mnUString = getStringFromMaybePellets mnU
            mnDString = getStringFromMaybePellets mnD
            mnLString = getStringFromMaybePellets mnL
            mnRString = getStringFromMaybePellets mnR
        in
            "Pellets at " 
            ++ show p ++ " {" 
            ++ mnUString ++ ", " 
            ++ mnDString ++ ", " 
            ++ mnLString ++ ", " 
            ++ mnRString ++ "}" 


instance Show PelletsGroup where
  show (PelletsGroup ns) =
    "PelletsGroup [ " ++ unwords (map (show . getValue) ns) ++ " ]"


getStringFromMaybePellets :: Maybe Pellets -> String
getStringFromMaybePellets Nothing  = "Nothing"
getStringFromMaybePellets (Just n) = show (getValue n)


p121 :: Point2 Int
p121 = point 100 80


n121 :: Pellets
n121 = mkPellets (point 100 80) ( Nothing, Nothing, Nothing, Nothing )



mkPellets :: Point -> ( Maybe Pellets, Maybe Pellets, Maybe Pellets, Maybe Pellets ) -> Pellets
mkPellets p (u, d, l, r) = Pellets { getValue=p, getU=u, getD=d , getL=l , getR=r }


getPellets :: PelletsGroup
getPellets = PelletsGroup [n121]


getFloatPointPelletsValue :: Pellets -> Point2 Float
getFloatPointPelletsValue (Pellets (Point x y) _ _ _ _) = point (fromIntegral x) (fromIntegral y)


{-drawPellets :: PelletsGroup -> DC a -> IO ()
drawPellets PelletsGroup dc = do
    drawWhitePoints PelletsGroup dc-}


getMaybePelletsValue :: Maybe Pellets -> Maybe Point
getMaybePelletsValue Nothing  = Nothing
getMaybePelletsValue (Just n) = Just (getValue n)


drawWhiteLine :: Point -> Point -> DC a -> IO ()
drawWhiteLine point1 point2 dc = do
    set dc [ penColor := white
           , penKind  := PenSolid
           , penWidth := 2
           ]
    dcDrawLine   dc point1 point2


drawWhitePoints :: PelletsGroup -> DC a -> IO ()
drawWhitePoints (PelletsGroup []) _       = return ()
drawWhitePoints (PelletsGroup (n:ns)) dc  = do
    let p = getValue n
    drawWhitePoint p dc

    drawWhitePoints (PelletsGroup ns) dc


drawWhitePoint :: Point -> DC a -> IO ()
drawWhitePoint p dc = do
    set dc [ brushColor := white
           , brushKind  := BrushSolid
           , penColor := white
           , penKind  := PenSolid
           , penWidth := 2
           ]
    let radius = 2
    dcDrawCircle dc p radius
