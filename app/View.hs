module View (module View) where

import Vector3

-- CONSTRUCT THROUGH WRAPPER FUNCTION

-- TODO: depth of field blur (aperture stuff, simulate camera focal point)
data Camera a = CameraInternal
  { vy :: Vec3 a,
    vx :: Vec3 a,
    vz :: Vec3 a
  }
  deriving (Show)

-- Note that vfov is the angle determining the width.
-- Heigth is always a ratio to the width.
createCamera :: Fractional a => Vec3 a -> Vec3 a -> a -> a -> Camera a
createCamera lookFrom lookAt vup = Camera vy vx vz
  where
    vz = unitVector $ lookAt - lookFrom
    vx = vz `cross` vy
    vy = unitVector $ vup - ((vup `dot` vz) `scalarMul` vz)
