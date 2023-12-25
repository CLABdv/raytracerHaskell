module View where

import Vector3

-- TODO: depth of field blur (aperture stuff, simulate camera focal point)
-- Use createCamera to construct. CameraInternal is only for internal usage.
data Camera = CameraInternal
  { vy :: Vec3 ,
    vx :: Vec3 ,
    vz :: Vec3 ,
    llc :: Vec3 ,
    width :: Double,
    height :: Double,
    resx :: Int,
    resy :: Int,
    pixelWidth :: Double,
    pixelHeight :: Double,
    apertureRadius :: Double
  }
  deriving (Show)

-- Note that vfov is the angle determining the width.
-- Height is always a ratio to the width.
-- the horror of this signature
createCamera :: Vec3 -> Vec3 -> Vec3 -> Double -> Double -> Int -> Double -> Camera
createCamera lookFrom lookAt vup height width resx = CameraInternal vy vx vz llc width height resx resy pxW pxH
  where
    vz = unitVector $ lookAt - lookFrom
    vx = vz `cross` vy
    vy = unitVector $ vup - ((vup `dot` vz) `scalarMul` vz)
    llc = lookAt - scalarMul (width / 2) vx - scalarMul (height / 2) vy
    resy = round ((fromIntegral resx / width) * height)
    pxW = width / fromIntegral resx
    pxH = height / fromIntegral resy

createRect :: Camera -> [[Vec3]]
createRect (CameraInternal vy vx _ llc _ _ resx resy pxW pxH _) =
  map (take resx . (\x -> genRow x $ scalarMul pxW vx)) firstCol
  where
    -- HACK: I reverse the list to make up be upwards instead of up being downwards.
    -- generates the columns furthest to the left.
    firstCol = reverse $ take resy $ genRow llc $ scalarMul pxH vy
    -- generates an infinite list of 3d vectors.
    -- basically [base,base+step..] if i had implemented Enum
    genRow base step = base : genRow (base + step) step
