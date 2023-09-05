{-# LANGUAGE InstanceSigs #-}
module Vector3
    ( Vector3 (Vector3)
    , magnitudeSquared
    , dot
    , magnitude) where

data Vector3 = Vector3
  { x :: !Double
  , y :: !Double
  , z :: !Double }
  deriving (Show, Eq)

instance Num Vector3 where
  (+) :: Vector3 -> Vector3 -> Vector3
  (Vector3 x y z) + (Vector3 x' y' z') = Vector3 (x+x') (y+y') (z+z')
  (*) :: Vector3 -> Vector3 -> Vector3
  (Vector3 x y z) * (Vector3 x' y' z') = Vector3 (x*x') (y*y') (z*z')
  abs :: Vector3 -> Vector3
  abs (Vector3 x y z) = Vector3 (abs x) (abs y) (abs z)
  signum :: Vector3 -> Vector3
  signum vec = if magnitudeSquared vec > 0 then 1 else 0
  fromInteger :: Integer -> Vector3
  fromInteger n = Vector3 x x x where x = fromIntegral n
  negate :: Vector3 -> Vector3
  negate (Vector3 x y z) = Vector3 (negate x) (negate y) (negate z)

instance Fractional Vector3 where
  fromRational :: Rational -> Vector3
  fromRational ra = Vector3 x x x where x = fromRational ra
  (/) :: Vector3 -> Vector3 -> Vector3
  (Vector3 x y z) / (Vector3 x' y' z') = Vector3 (x/x') (y/y') (z/z')

magnitudeSquared :: Vector3 -> Double
magnitudeSquared (Vector3 x y z) = x*x + y*y + z*z

magnitude :: Vector3 -> Double
magnitude = sqrt . magnitudeSquared

infixl 7 `dot`
dot :: Vector3 -> Vector3 -> Double
dot (Vector3 x y z) (Vector3 x' y' z') = x*x' + y*y' + z*z'
