module Main where

import Control.Monad
import Helpers
import LightRay
import Shapes
import System.Random
import Vector3
import View

main :: IO ()
main = do
  {-
  TODO:
  Make the FOV be in radians/degrees instead of a given thingy
  Wrap camera stuff in a camera module
  Create a error when the plane up is the exact same direction as the vector from lookfrom to lookat.
  Maybe implement enum for vectors to be able to do a [1,a..] instead of calling genRow
  make it so that the up vector is at a square angle to the lookat vector

  For light, make it so that bounced rays have a bias to go towards lightsources, which depends on the sources strengths.
  (or something)
  -}
  seed <- randomIO :: IO Int
  let lookfrom = Vec3 (-10) 3.2 (-18) :: Vec3 Float -- lookfrom, lookat, width, height and vup are magic numbers. so is resx
      lookat = Vec3 0 2.5 0 :: Vec3 Float

      star = Sphere 1000000000 (Vec3 0 1000001000 0) (Lightsource (Vec3 1 1 1))
      ground = Sphere 10000 (Vec3 0 (-10000) 0) (Lambertian (Vec3 0.5 0.5 0.5))
      bb1 = Sphere 4 (Vec3 (-8) 4 8) (Lambertian (Vec3 0.8 0.3 0.9))
      bb2 = Sphere 4 (Vec3 0 4 0) (Refractive 1.5)
      bb3 = Sphere 4 (Vec3 8 4 (-8)) (Specular (Vec3 0.9 0.9 0.9) 0)

      noise =
        runRandom
          ( do
              mat <- replicateM 220 randMat
              pos <- randPos (-15, -15) 15
              return $ zipWith (Sphere 1) pos mat
          )
          seed
      spheres = [ground, star, bb1, bb2, bb3] ++ noise

      width = 40 :: Float
      height = 25 :: Float
      vy = unitVector $ Vec3 0 1 0 :: Vec3 Float -- insert your desired y axis
      resx = 400 :: Int -- FOV width in actual pixels
      resy = round ((fromIntegral resx / width) * height)

      cam = createCamera lookfrom lookat vy height width resx 0.1
      rect = createRect cam
      -- NOTE: The reason for these rays not being unitVectors, is since
      -- for antialiasing to work i need to have a full lightray to the pixel, to be able to modify the
      -- direction of the ray to be a random point inside that pixel.
      -- There probably is a better way to do this, but this'll work for now.
      rays = concatMap (map (\x -> Ray (x - lookfrom) lookfrom (Vec3 1 1 1))) rect

      rpp = 10 :: Int
      bounces = 8 :: Int
      cols = coloursPar cam spheres bounces rpp (randoms (mkStdGen seed)) rays
      finCols = map (roundVec . vecToCol) cols
  putStrLn $ ppmWrite resx resy finCols

ppmWrite :: Int -> Int -> [Vec3 Int] -> String
ppmWrite w h i = "P3" ++ " " ++ show w ++ " " ++ show h ++ "\n" ++ "255\n" ++ writeInfo i
  where
    writeInfo [] = []
    writeInfo ((Vec3 r g b) : cs) = show r ++ " " ++ show g ++ " " ++ show b ++ "\n" ++ writeInfo cs

-- generates an infinite list of 3d vectors.
-- basically [base,base+step..] if i had implemented Enum
genRow :: Num a => Vec3 a -> Vec3 a -> [Vec3 a]
genRow base step = base : genRow (base + step) step

vecToCol :: (Num a, Ord a) => Vec3 a -> Vec3 a
vecToCol (Vec3 a b c) = scalarMul 255 (Vec3 (max a 0) (max b 0) (max c 0))

roundVec :: (RealFrac a) => Vec3 a -> Vec3 Int
roundVec (Vec3 a b c) = Vec3 (round a) (round b) (round c)

-- gets x,z coords of lowest corner
-- returns inf list, make sure to take a part of the spine of it
-- otherwise eval wont finish til' after the heat death of the universe.
randPos :: (Enum a, Random a, Floating a) => (a, a) -> Int -> R [Vec3 a]
randPos (lx, lz) c = do
  let l = [(x, z) | x <- take c [lx, lx + 6 ..], z <- take c [lz, lz + 6 ..]]
  mapM
    ( \(a, b) -> do
        offsetx <- rand
        offsety <- rand
        return $ Vec3 (a + 6 * (offsetx - 0.5)) 1 (b + 6 * (offsety - 0.5))
    ) l
