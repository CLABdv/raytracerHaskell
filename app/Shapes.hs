module Shapes (module Shapes) where

import Vector3

-- Material colours should be in the interval [0,1]
data Material a
  = Lambertian (Vec3 a)
  | Lightsource (Vec3 a)
  | Specular -- TODO: Make it so that a specular material also has a colour and also has a variable for how diffuse the reflection is
  | Refractive a -- Any material which refracts light. TODO: Make so that a material can be partially refractive, make so the material can have a colour. Currently always assumes "air" is vacuum
  deriving (Show, Eq)

-- sphere with a center and a radius
data Object a = Sphere a (Vec3 a) (Material a)
  deriving (Show, Eq)
