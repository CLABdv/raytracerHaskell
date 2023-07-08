module Vector3 (module Vector3) where

-- TODO:
-- Implement scalarDiv

import Helpers
import System.Random (Random)

-- X Y Z
-- Three dimensional vector of floating points.
data Vec3 a = Vec3 {x :: !a, y :: !a, z :: !a}
  deriving (Show, Eq)

instance Num a => Num (Vec3 a) where
  Vec3 a b c + Vec3 i j k = Vec3 (a + i) (b + j) (c + k)
  Vec3 a b c - Vec3 i j k = Vec3 (a - i) (b - j) (c - k)

  Vec3 a b c * Vec3 i j k =
    error
      "Do not use '*' to multiply two vectors. use cross or dot instead (or if with a scalar, scalarMul)"
  abs (Vec3 a b c) = Vec3 (abs a) (abs b) (abs c)
  signum (Vec3 a b c) = Vec3 (signum a) (signum b) (signum c)
  fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i)
  negate (Vec3 a b c) = Vec3 (-a) (-b) (-c)

-- cross multiplication of two vectors
cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 a b c) (Vec3 i j k) = Vec3 (b * k - c * j) (i * c - a * k) (a * j - b * i)

-- dot multiplication of two vectors
dot :: Num a => Vec3 a -> Vec3 a -> a
dot (Vec3 a b c) (Vec3 i j k) = a * i + b * j + c * k

scalarMul :: Num a => a -> Vec3 a -> Vec3 a
scalarMul s (Vec3 a b c) = Vec3 (a * s) (b * s) (c * s)

scalarDiv :: Fractional a => a -> Vec3 a -> Vec3 a
scalarDiv s (Vec3 a b c) = Vec3 (a / s) (b / s) (c / s)

-- gives the squared vector length. This is useful cuz taking the square root is slow and is not always needed
sqVecLen :: Floating a => Vec3 a -> a
sqVecLen (Vec3 a b c) = a ^ 2 + b ^ 2 + c ^ 2

vecLen :: Floating a => Vec3 a -> a
vecLen = sqrt . sqVecLen

-- Produce a unit vector given a vector
unitVector :: Floating a => Vec3 a -> Vec3 a
unitVector r@(Vec3 a b c) = Vec3 (a / l) (b / l) (c / l)
  where
    l = vecLen r

randVec :: Random a => R (Vec3 a)
randVec = do
  x <- rand
  y <- rand
  z <- rand
  return $ Vec3 x y z

{-
Returns a vector of length < 1

I think this method clumps the distribution of the vectors around the equator and the poles,
therefore TODO: implement another method
Probability of length < 1 is about 0.52.

also TODO: make it polymorphic instead of Float
-}
randVecInUnitSphere :: (Ord a, Floating a, Random a) => R (Vec3 a)
randVecInUnitSphere = do
  v <- randVec
  let l = vecLen v
  if l < 1 then return v else randVecInUnitSphere
