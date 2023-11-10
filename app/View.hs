module View where

import Vector3

-- TODO: depth of field blur (aperture stuff, simulate camera focal point)
-- Use createCamera to construct. CameraInternal is only for internal usage.
data Camera a = CameraInternal
  { vy :: Vec3 a,
    vx :: Vec3 a,
    vz :: Vec3 a,
    llc :: Vec3 a,
    width :: a,
    height :: a,
    resx :: Int,
    resy :: Int,
    pixelWidth :: a,
    pixelHeight :: a,
    apertureRadius :: a
  }
  deriving (Show)

-- Note that vfov is the angle determining the width.
-- Height is always a ratio to the width.
-- the horror of this signature
createCamera :: (RealFrac a, Floating a) => Vec3 a -> Vec3 a -> Vec3 a -> a -> a -> Int -> a -> Camera a
createCamera lookFrom lookAt vup height width resx = CameraInternal vy vx vz llc width height resx resy pxW pxH
  where
    vz = unitVector $ lookAt - lookFrom
    vx = vz `cross` vy
    vy = unitVector $ vup - ((vup `dot` vz) `scalarMul` vz)
    llc = lookAt - scalarMul (width / 2) vx - scalarMul (height / 2) vy
    resy = round ((fromIntegral resx / width) * height)
    pxW = width / fromIntegral resx
    pxH = height / fromIntegral resy

createRect :: RealFrac a => Camera a -> [[Vec3 a]]
createRect (CameraInternal vy vx _ llc _ _ resx resy pxW pxH _) =
  map (take resx . (\x -> genRow x $ scalarMul pxW vx)) firstCol
  where
    -- HACK: I reverse the list to make up be upwards instead of up being downwards.
    -- generates the columns furthest to the left.
    firstCol = reverse $ take resy $ genRow llc $ scalarMul pxH vy
    -- generates an infinite list of 3d vectors.
    -- basically [base,base+step..] if i had implemented Enum
    genRow base step = base : genRow (base + step) step
