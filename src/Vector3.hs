{-# LANGUAGE InstanceSigs #-}
module Vector3
  ( Vector3 (Vector3)
  , magnitudeSquared
  , dot
  , toVec
  , magnitude
  , normalize
  , projectOn) where

data Vector3 = Vector3
  !Double -- ^ x coord
  !Double -- ^ y coord
  !Double -- ^ z coord
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
  fromInteger n = toVec x where x = fromIntegral n
  negate :: Vector3 -> Vector3
  negate (Vector3 x y z) = Vector3 (negate x) (negate y) (negate z)

instance Fractional Vector3 where
  fromRational :: Rational -> Vector3
  fromRational ra = toVec x where x = fromRational ra
  (/) :: Vector3 -> Vector3 -> Vector3
  (Vector3 x y z) / (Vector3 x' y' z') = Vector3 (x/x') (y/y') (z/z')

toVec :: Double -> Vector3
toVec n = Vector3 n n n

magnitudeSquared :: Vector3 -> Double
magnitudeSquared (Vector3 x y z) = x*x + y*y + z*z

magnitude :: Vector3 -> Double
magnitude = sqrt . magnitudeSquared

infixl 7 `dot`
dot :: Vector3 -> Vector3 -> Double
dot (Vector3 x y z) (Vector3 x' y' z') = x*x' + y*y' + z*z'

normalize :: Vector3 -> Vector3
normalize vec = vec * toVec (1/magnitude vec)

-- | Project the first vector onto the second one.
infix 8 `projectOn`
projectOn :: Vector3 -> Vector3 -> Vector3
b `projectOn` a = toVec ((a `dot` b) / (a `dot` a)) * a
