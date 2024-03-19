module Vector3 (module Vector3) where

-- BUG
-- IMPORTANT:
-- Random vector utilities do not generate negative numbers. Numbers are in range [0,1[
-- Change this to be [-1,1]

-- TODO:
-- Implement scalarDiv

import Control.DeepSeq
import Helpers
import System.Random (Random)

-- X Y Z
-- Three dimensional vector of floating points.
data Vec3 = Vec3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Show, Eq)

instance Num Vec3 where
  Vec3 a b c + Vec3 i j k = Vec3 (a + i) (b + j) (c + k)
  {-# INLINE (+) #-}
  Vec3 a b c - Vec3 i j k = Vec3 (a - i) (b - j) (c - k)
  {-# INLINE (-) #-}

  Vec3 a b c * Vec3 i j k = Vec3 (a * i) (b * j) (c * k) -- Elementwise multiplication.
  {-# INLINE (*) #-}
  abs (Vec3 a b c) = Vec3 (abs a) (abs b) (abs c)
  signum (Vec3 a b c) = Vec3 (signum a) (signum b) (signum c)
  fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i)
  negate (Vec3 a b c) = Vec3 (-a) (-b) (-c)

instance NFData Vec3 where
  rnf (Vec3 i j k) = i `deepseq` j `deepseq` k `deepseq` ()

vecMap :: (Double -> Double) -> Vec3 -> Vec3
vecMap f (Vec3 a b c) = Vec3 (f a) (f b) (f c)

vecZipWith :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
vecZipWith f (Vec3 a b c) (Vec3 i j k) = Vec3 (f a i) (f b j) (f k c)

-- cross multiplication of two vectors
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a b c) (Vec3 i j k) = Vec3 (b * k - c * j) (i * c - a * k) (a * j - b * i)
{-# INLINE cross #-}

-- dot multiplication of two vectors
dot :: Vec3 -> Vec3 -> Double
dot (Vec3 a b c) (Vec3 i j k) = a * i + b * j + c * k
{-# INLINE dot #-}

elemInverse :: Vec3 -> Vec3
elemInverse (Vec3 a b c) = Vec3 (1 / a) (1 / b) (1 / c)
{-# INLINE elemInverse #-}

scalarMul :: Double -> Vec3 -> Vec3
scalarMul s (Vec3 a b c) = Vec3 (a * s) (b * s) (c * s)
{-# INLINE scalarMul #-}

scalarDiv :: Double -> Vec3 -> Vec3
scalarDiv s (Vec3 a b c) = Vec3 (a / s) (b / s) (c / s)
{-# INLINE scalarDiv #-}

-- gives the squared vector length. This is useful cuz taking the square root is slow and is not always needed
sqVecLen :: Vec3 -> Double
sqVecLen (Vec3 a b c) = a * a + b * b + c * c
{-# INLINE sqVecLen #-}

vecLen :: Vec3 -> Double
vecLen = sqrt . sqVecLen
{-# INLINE vecLen #-}

-- Produce a unit vector given a vector
unitVector :: Vec3 -> Vec3
unitVector r@(Vec3 a b c) = Vec3 (a / l) (b / l) (c / l)
  where
    l = vecLen r
{-# INLINE unitVector #-}

randVec = do
  x <- rand
  y <- rand
  z <- rand
  return $ Vec3 x y z

randInUnitDisc :: (Floating a, Ord a, Random a) => R (a, a)
randInUnitDisc = do
  x <- rand
  y <- rand
  let a = x
      b = y
  if a * a + b * b < 1
    then return (a, b)
    else randInUnitDisc

{-
Returns a vector of length < 1

I think this method clumps the distribution of the vectors around the equator and the poles,
therefore TODO: implement another method
Probability of length < 1 is about 0.52.

also TODO: make it polymorphic instead of Float
-}
randVecInUnitSphere = do
  v <- randVec
  let l = vecLen v
  if l < 1 then return v else randVecInUnitSphere

randUnitVec = do
  t <- randVecInUnitSphere
  (return . unitVector) t
{-# INLINE randUnitVec #-}
