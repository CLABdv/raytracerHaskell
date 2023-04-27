module Main where
import Data.Maybe
import Vector3
import Shapes
import LightRay
import Helpers
import System.Random (Random, StdGen, mkStdGen, random, randomIO)

main :: IO ()
main = do
{-
TODO:
Make the FOV be in radians/degrees instead of a given thingy
Wrap camera stuff in a camera module
Create a error when the plane up is the exact same direction as the vector from lookfrom to lookat.
Maybe implement enum for vectors to be able to do a [1,a..] instead of calling genRow

Make so that you can change the intensity of the lightrays from certain objects. Only cap the colours at the very end, then everything above 1 is truncated to 1.
Issue with this is that it probably is required to send a buncha rays for it to look good
-}
  seed <- randomIO :: IO Int
  let lookfrom = Vec3 (0,4,8) :: Vec3 Float Float Float --lookfrom, lookat, width, height and vup are magic numbers. so is resx
      lookat   = Vec3 (0,0,0) :: Vec3 Float Float Float

      star     = Sphere 1000000000 (Vec3 (0,1000001000,0)) (Lightsource (Vec3 (1,1,1)))
      s1      = Sphere 1 (Vec3 (0,1,0)) (Lambertian (Vec3 (0.5,0.5,0.5)))
      s2      = Sphere 1 (Vec3 (2,1,0)) Specular
      ground  = Sphere 10000 (Vec3 (0,-10000,0)) (Lambertian (Vec3 (0.2,0.8,0.2)))

      spheres  = [ground,s1,star,s2]

      width    = 10 :: Float
      height   = 5 :: Float
      vup      = unitVector $ Vec3 (0,1,0) :: Vec3 Float Float Float -- basically y axis but for the FOV
      w        = unitVector $ lookat - lookfrom
      u        = unitVector $ cross w vup -- basically x axis but for the FOV
      resx     = 400 :: Int -- FOV width in actual pixels
      resy     = round ((fromIntegral resx/width) * height)
      -- lower left corner. the lookat position minus half the width times the 'x' unit vector minus half the height times the 'y' unit vector.
      -- quotation marks 'cause its not really x and y vectors, it's those vectors mapped to the plane.
      llc      = lookat - scalarMul (width*0.5) u - scalarMul (height*0.5) vup :: Vec3 Float Float Float
      -- HACK: I reverse the list to make up be upwards instead of up being downwards.
      firstCol = reverse $ take resy $ genRow llc $ scalarMul (height/fromIntegral resy) vup -- generates the columns furthest to the left.

      rect     = map (take resx.(\x -> genRow x $ scalarMul (width/fromIntegral resx) u)) firstCol -- full rectangle of all points from bot left to top right
      rays = concatMap (map (\x -> Ray (x - lookfrom) lookfrom (Vec3 (1,1,1)))) rect

      cols = runRandom (colours spheres rays 10 10) seed
      finCols = map (roundVec . vecToCol) cols
  putStrLn $ ppmWrite resx resy finCols

ppmWrite :: Int -> Int -> [Vec3 Int Int Int] -> String
ppmWrite w h i = "P3" ++ " " ++ show w ++ " " ++ show h ++ "\n" ++ "255\n" ++ writeInfo i
  where writeInfo []     = []
        writeInfo (Vec3 (r,g,b):cs) = show r ++ " " ++ show g ++ " " ++ show b ++ "\n" ++ writeInfo cs

-- generates an infinite list of 3d vectors.
-- basically [base,base+step..] if i had implemented Enum
genRow :: Num a => Vec3 a a a -> Vec3 a a a -> [Vec3 a a a]
genRow base step = base : genRow (base+step) step

vecToCol :: (Num a, Ord a) => Vec3 a a a -> Vec3 a a a
vecToCol (Vec3 (a,b,c)) = scalarMul 255 (Vec3 (max a 0, max b 0, max c 0)) -- if negative, instead do 0

roundVec :: (RealFrac a, Integral b) => Vec3 a a a -> Vec3 b b b
roundVec (Vec3 (a,b,c)) = Vec3 (round a, round b, round c)
