{-# LANGUAGE DeriveFunctor #-}

module Vec_old
    (   Vec(..)
        , getX              -- :: Vec a -> a
        , getY              -- :: Vec a -> a
        , thresh            -- :: Vec a -> a
        , mkVec             -- :: (Num a, Ord a) => a -> a -> a -> Vec a
        , mkVecFloat        -- :: Num a => a -> a -> Vec a
        , mkVecFromPoint    -- :: Point -> Vec Float
        , magnitudeSquared  -- :: Num a     => Vec a -> a
        , magnitude         -- :: Floating a => Vec a -> a
        , manhattenDistance2 -- :: :: Num a => Vec a -> a
    ) where
import Graphics.UI.WXCore



-- TODO: change Vec to be without thresh and use thresh from Constants.hs
data Vec a = Vec a a Float
    deriving (Show, Functor)

getX :: Vec a -> a
getX (Vec x _ _) = x

getY :: Vec a -> a
getY (Vec _ y _) = y

thresh :: Vec a -> Float
thresh (Vec _ _ t) = t

mkVec :: (Num a, Ord a) => a -> a -> Float -> Vec a
mkVec x y t = Vec x y (max 0 t)

mkVecFloat :: Float -> Float -> Vec Float
mkVecFloat x y = Vec x y 0.1
-- mkVecFloat x y = Vec x y 0.000001

mkVecFromPoint :: Point -> Vec Float
mkVecFromPoint (Point x y) = Vec (fromIntegral x) (fromIntegral y) 0.000001

-- pointToVec :: Point2 Float -> Vec Float
-- pointToVec (Point x y) = mkVecFloat x y

instance Real a => Eq (Vec a) where
    Vec x1 y1 t1 == Vec x2 y2 t2 =
        realToFrac (abs (x1 - x2)) < min t1 t2 &&
        realToFrac (abs (y1 - y2)) < min t1 t2

instance (Num a, Ord a) => Num (Vec a) where
    (+) (Vec x1 y1 t1) (Vec x2 y2 t2) =
        Vec (x1 + x2) (y1 + y2) (min t1 t2)

    (-) (Vec x1 y1 t1) (Vec x2 y2 t2) =
        Vec (x1 - x2) (y1 - y2) (min t1 t2)

    (*) (Vec x1 y1 t1) (Vec x2 y2 t2) =
        Vec (x1 * x2) (y1 * y2) (min t1 t2)

    negate  (Vec x y t) = Vec (negate x) (negate y) t
    abs     (Vec x y t) = Vec (abs x)    (abs y)    t
    signum  (Vec x y t) = Vec (signum x) (signum y) t

    fromInteger n =
        let f = fromInteger n
        in Vec f f (abs (fromIntegral n))

instance (Fractional a, Ord a) => Fractional (Vec a) where
    (/) (Vec x1 y1 t1) (Vec x2 y2 t2) =
        Vec (x1 / x2) (y1 / y2) (min t1 t2)

    fromRational r =
        let f = fromRational r
        in Vec f f (abs (realToFrac r))

manhattenDistance2 :: Num a => Vec a -> a
manhattenDistance2 (Vec x y _) = (abs x) + (abs y)

magnitudeSquared :: Num a => Vec a -> a
magnitudeSquared (Vec x y _) = x*x + y*y

magnitude :: Floating a => Vec a -> a
magnitude v = sqrt (magnitudeSquared v)
