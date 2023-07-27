module View where

import Vector3

-- TODO: depth of field blur (aperture stuff, simulate camera focal point)
-- Obviously dont use CameraInternal to construct shit outside this stuff
data Camera a = CameraInternal
  { vy :: Vec3 a,
    vx :: Vec3 a,
    vz :: Vec3 a,
    llc :: Vec3 a,
    width :: a,
    height :: a,
    resx :: Int
  }
  deriving (Show)

-- Note that vfov is the angle determining the width.
-- Height is always a ratio to the width.
createCamera lookFrom lookAt vup height width resx = CameraInternal vy vx vz llc width height resx
  where
    vz = unitVector $ lookAt - lookFrom
    vx = vz `cross` vy
    vy = unitVector $ vup - ((vup `dot` vz) `scalarMul` vz)
    llc = lookAt - scalarMul (width / 2) vx - scalarMul (height / 2) vy

createRect (CameraInternal vy vx _ llc width height resx) = map (take resx . (\x -> genRow x $ scalarMul (width / fromIntegral resx) vx)) firstCol
  where
    -- HACK: I reverse the list to make up be upwards instead of up being downwards.
    -- generates the columns furthest to the left.
    firstCol = reverse $ take resy $ genRow llc $ scalarMul (height / fromIntegral resy) vy
    -- generates an infinite list of 3d vectors.
    -- basically [base,base+step..] if i had implemented Enum
    genRow base step = base : genRow (base + step) step
    resy = round ((fromIntegral resx / width) * height)
