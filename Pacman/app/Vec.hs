{-# LANGUAGE DeriveFunctor #-}

module Vec
    (   Vec(..)
        , getX                  -- :: Vec a -> a
        , getY                  -- :: Vec a -> a
        , mkVec                 -- :: (Num a, Ord a) => a -> a -> a -> Vec a
        , mkVecFromPoint        -- :: Point -> Vec Float
        , getDirectionVec       -- :: String -> Vec Float
        , scaleVec2             -- :: Float -> Float -> Vec Float -> Vec Float
        , magnitudeSquared      -- :: Num a     => Vec a -> a
        , manhattenDistance     -- :: Num a => Vec a -> Vec a -> a
    ) where
import Graphics.UI.WXCore

import qualified Constants as C



data Vec a = Vec a a
    deriving (Show, Functor)


instance Real a => Eq (Vec a) where
    Vec x1 y1 == Vec x2 y2 =
        realToFrac (abs (x1 - x2)) < C.thresh &&
        realToFrac (abs (y1 - y2)) < C.thresh


instance (Num a, Ord a) => Num (Vec a) where
    (+) (Vec x1 y1) (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)
    (-) (Vec x1 y1) (Vec x2 y2) = Vec (x1 - x2) (y1 - y2)
    (*) (Vec x1 y1) (Vec x2 y2) = Vec (x1 * x2) (y1 * y2)

    negate  (Vec x y) = Vec (negate x) (negate y)
    abs     (Vec x y) = Vec (abs x)    (abs y)
    signum  (Vec x y) = Vec (signum x) (signum y)

    fromInteger n =
        let f = fromInteger n
        in Vec f f


instance (Fractional a, Ord a) => Fractional (Vec a) where
    (/) (Vec x1 y1) (Vec x2 y2) = Vec (x1 / x2) (y1 / y2)

    fromRational r =
        let f = fromRational r
        in Vec f f



getX :: Vec a -> a
getX (Vec x _) = x

getY :: Vec a -> a
getY (Vec _ y) = y

mkVec :: (Num a, Ord a) => a -> a -> Vec a
mkVec x y = Vec x y

mkVecFromPoint :: Point -> Vec Float
mkVecFromPoint (Point x y) = Vec (fromIntegral x) (fromIntegral y)

getDirectionVec :: String -> Vec Float
getDirectionVec direction = case direction of
    "u" -> Vec   0.0 (-1.0)
    "d" -> Vec   0.0   1.0
    "l" -> Vec (-1.0)  0.0
    "r" -> Vec   1.0   0.0
    _   -> Vec   0.0   0.0

scaleVec2 :: Float -> Float -> Vec Float -> Vec Float
scaleVec2 scalar1 scalar2 (Vec x y) =
    let 
        scalar = scalar1 * scalar2
    in
        Vec (scalar * x) (scalar * y)

manhattenDistance :: Num a => Vec a -> Vec a -> a
manhattenDistance (Vec x1 y1) (Vec x2 y2) = (abs (x1 - x2)) + (abs (y1 - y2))

magnitudeSquared :: Num a => Vec a -> a
magnitudeSquared (Vec x y) = x*x + y*y
