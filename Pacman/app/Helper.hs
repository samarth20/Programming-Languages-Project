module Helper where
import Graphics.UI.WX


isAlmostEqual :: Point -> Point -> Int -> Bool
isAlmostEqual (Point x1 y1) (Point x2 y2) thresh = 
    abs (x1 - x2) <= thresh && abs (y1 - y2) <= thresh


rgb3 :: (Int,Int,Int) -> Color
rgb3 (r,g,b) = rgb r g b
