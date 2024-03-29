module Main where

import qualified BoundingBoxes as B
import Control.Monad
import qualified Data.Vector as V
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
  Create a error when the plane up is the exact same direction as the vector from lookfrom to lookat.

  For light, make it so that lightsources dont look all fucky
  Maybe rework how the lightsources work all togheter

  Implement bounding boxes
  -}
  seed <- randomIO :: IO Int
  -- print seed
  let lookfrom = Vec3 (-10, 3.2, -18)
      lookat = Vec3 (0, 2.5, 0)

      star = Sphere 10 (Vec3 (0, 20, 0)) (Lightsource (Vec3 (2, 2, 2)))
      star2 = Sphere 10 (Vec3 (10, 20, 18)) (Lightsource (Vec3 (2, 2, 2)))
      ground = Sphere 10000 (Vec3 (0, -10000, 0)) (Lambertian (Vec3 (0.5, 0.5, 0.5)))
      bb1 = Sphere 4 (Vec3 (-8, 4, 8)) (Lambertian (Vec3 (0.8, 0.3, 0.9)))
      bb2 = Sphere 4 (Vec3 (0, 4, 0)) (Refractive 1.5)
      bb3 = Sphere 4 (Vec3 (8, 4, -8)) (Specular (Vec3 (0.9, 0.9, 0.9)) 0)

      noise =
        runRandom
          ( do
              mat <- replicateM 220 randMat
              pos <- randPos (-15, -15) 15
              return $ zipWith (Sphere 1) pos mat
          )
          seed
      spheres = [ground, star, star2, bb1, bb2, bb3] ++ noise
      box = runRandom (B.constructor spheres) seed
      width = 40
      height = 25
      vy = unitVector $ Vec3 (0, 1, 0)
      resx = 200 :: Int -- FOV width in actual pixels
      resy = round (fromIntegral resx / width * height)

      cam = createCamera lookfrom lookat vy height width resx 0.1
      rect = createRect cam
      -- rect = createVecRect cam
      -- NOTE: The reason for these rays not being unitVectors, is since
      -- for antialiasing to work i need to have a full lightray to the pixel, to be able to modify the
      -- direction of the ray to be a random point inside that pixel.
      -- There probably is a better way to do this, but this'll work for now.
      rays = map (\x -> Ray (x - lookfrom) lookfrom 1) rect
      -- rays = V.map (\x -> Ray (x - lookfrom) lookfrom 1) rect

      rpp = 1 :: Int
      bounces = 8 :: Int
      cols = runRandom (colours cam box bounces rpp rays) seed
      finCols = fmap (truncVec . vecToCol) cols
  putStrLn $ ppmWrite resx resy finCols

ppmWrite :: Foldable t => Int -> Int -> t (Int, Int, Int) -> String
ppmWrite w h i = "P3" ++ " " ++ show w ++ " " ++ show h ++ "\n" ++ "255\n" ++ 
    foldr (\(r, g, b) acc -> acc ++ show r ++ " " ++ show g ++ " " ++ show b ++ "\n") "" i

genRow :: Vec3 -> Vec3 -> [Vec3]
genRow base step = base : genRow (base + step) step

vecToCol :: Vec3 -> Vec3
vecToCol (Vec3 (a, b, c)) = scalarMul 255 (Vec3 (min (max a 0) 1, min (max b 0) 1, min (max c 0) 1))

truncVec :: Vec3 -> (Int, Int, Int)
truncVec (Vec3 (a, b, c)) = (truncate a, truncate b, truncate c)

-- gets x,z coords of lowest corner
-- returns inf list, make sure to take a part of the spine of it
-- otherwise eval wont finish til' after the heat death of the universe.
randPos :: (Double, Double) -> Int -> R [Vec3]
randPos (lx, lz) c = do
  let l = [(x, z) | x <- take c [lx, lx + 6 ..], z <- take c [lz, lz + 6 ..]]
  mapM
    ( \(a, b) -> do
        offsetx <- rand
        offsety <- rand
        return $ Vec3 (a + 6 * (offsetx - 0.5), 1, b + 6 * (offsety - 0.5))
    )
    l
