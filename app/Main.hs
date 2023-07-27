module Main where

import Helpers (runRandom)
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
  fix the random generator, it's slow and noticably not random enough
  -}
  seed <- randomIO :: IO Int
  let lookfrom = Vec3 (-5.2) 1.2 (-1.2) :: Vec3 Float -- lookfrom, lookat, width, height and vup are magic numbers. so is resx
      lookat = Vec3 0 1 (-1) :: Vec3 Float

      star = Sphere 1000000000 (Vec3 0 1000001000 0) (Lightsource (Vec3 1 1 1))
      s1 = Sphere 1 (Vec3 0 1 0) (Lambertian (Vec3 0.5 0.5 0.5))
      s2 = Sphere 1 (Vec3 0 1 (-2)) (Refractive 1.5)
      s3 = Sphere 1 (Vec3 0 1 2) Specular
      ground = Sphere 10000 (Vec3 0 (-10000) 0) (Lambertian (Vec3 0.2 0.8 0.2))

      spheres = [ground, s1, star, s2, s3]

      width = 20 :: Float
      height = 10 :: Float
      vy = unitVector $ Vec3 1 1 0 :: Vec3 Float -- insert your desired y axis
      resx = 800 :: Int -- FOV width in actual pixels
      resy = round ((fromIntegral resx / width) * height)

      cam = createCamera lookfrom lookat vy height width resx
      rect = createRect cam
      rays = concatMap (map (\x -> Ray (x - lookfrom) lookfrom (Vec3 1 1 1))) rect

      rpp = 10 :: Int
      bounces = 8 :: Int
      cols = coloursPar spheres bounces rpp (randoms (mkStdGen seed)) rays
      -- cols = runRandom (colours spheres bounces rpp rays) seed
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

roundVec :: (RealFrac a, Integral b) => Vec3 a -> Vec3 b
roundVec (Vec3 a b c) = Vec3 (round a) (round b) (round c)
