module Shapes (module Shapes) where

import Helpers
import System.Random (Random)
import Vector3

-- Material colours should be in the interval [0,1]
data Material a
  = Lambertian (Vec3 a)
  | Lightsource (Vec3 a)
  | Specular (Vec3 a) a -- Colour; Fuzz
  | Refractive a -- Any material which refracts light. TODO: Make so that a material can be partially refractive, make so the material can have a colour. Currently always assumes "air" is vacuum
  deriving (Show, Eq)

-- sphere with a center and a radius
data Object a = Sphere a (Vec3 a) (Material a)
  deriving (Show, Eq)

-- Generates material data for an object.
-- Weights are not equal and lightsources can not be generated.
randMat :: (Random a, Floating a) => R (Material a)
randMat = do
  matPick <- rand :: R Float
  c <- randVec
  v <- rand
  if matPick < 0.75
    then return $ Lambertian c
    else
      if matPick < 0.90
        then return $ Specular c v
        else return $ Refractive 1.5
