module Shapes (module Shapes) where

import Helpers
import Vector3

-- Material colours should be in the interval [0,1]
data Material
  = Lambertian {-# UNPACK #-} !Vec3
  | Lightsource {-# UNPACK #-} !Vec3
  | Specular {-# UNPACK #-} !Vec3 {-# UNPACK #-} !Double -- Colour; Fuzz
  | Refractive {-# UNPACK #-} !Double -- Any material which refracts light. TODO: Make so that a material can be partially refractive, make so the material can have a colour. Currently always assumes "air" is vacuum
  deriving (Show, Eq)

-- sphere with a center and a radius
data Object = Sphere {-# UNPACK #-} !Double {-# UNPACK #-} !Vec3 !Material
  deriving (Show, Eq)

-- Generates material data for an object.
-- Weights are not equal and lightsources can not be generated.
randMat = do
  matPick <- rand :: R Double
  c <- randVec
  v <- rand
  if matPick < 0.75
    then return $ Lambertian c
    else
      if matPick < 0.90
        then return $ Specular c v
        else return $ Refractive 1.5
