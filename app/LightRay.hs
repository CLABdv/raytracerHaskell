module LightRay (module LightRay) where
import Shapes
import Vector3
import Helpers
import Data.Maybe (isNothing, isJust)
import System.Random (Random)
import Control.Monad (replicateM)
import Data.List (transpose)

{-
TODO:
stuff
-}

data Ray a = Ray {dir :: Vec3 a a a, origin :: Vec3 a a a, col :: Vec3 a a a}
  deriving (Show, Eq)

-- Checks for collision with object. Applies equation of a sphere and gets intersections with abc formula
collide :: (Ord a, Floating a) => Object a -> Ray a -> Maybe (Vec3 a a a)
collide (Sphere r (Vec3 (mx,my,mz)) _) (Ray v@(Vec3 (vx,vy,vz)) o@(Vec3 (ox,oy,oz)) _)
  -- the checks for K being positive checks if the lightrays actually travel in the vectors direction, if its negative they travel backwards
  | det < 0 = Nothing
  | vecLen d1 < vecLen d2 && k1 >= 0 = Just (d1 + o)
  | k2 >= 0 = Just (d2 + o)
  | otherwise = Nothing
  where a = vx^2+vy^2+vz^2
        b = 2 * vx * (ox - mx) + 2 * vy * (oy - my) + 2 * vz * (oz - mz)
        c = (sum . map (^2) $ zipWith (-) [mx,my,mz] [ox,oy,oz]) - r^2
        det = b^2 - 4*a*c
        detSqrt = sqrt det
        k1 = (-b+detSqrt)/(2*a)
        k2 = (-b-detSqrt)/(2*a)
        d1 = scalarMul k1 v
        d2 = scalarMul k2 v


-- Given an object and its point of intersection, give the normal vector of the tangent plane
-- NOTE: Maybe remove unitvector from this function since that is not always desired.
gradient :: Floating a => Object a -> Vec3 a a a -> Vec3 a a a
gradient (Sphere _ c _) p = unitVector $ p - c

-- Given a list of objects and a ray, checks which object is the closest to the rays origin.
collideObjects :: (Ord a, Floating a) => [Object a] -> Ray a -> Maybe (Object a, Vec3 a a a)
collideObjects o r@(Ray _ origin _)    = closest $ (filter (isJust . snd) . zip o) $ map (`collide` r) o
  where closest []                     = Nothing
        closest ((_, Nothing):ovs)     = closest ovs
        closest ((object, Just p):ovs) = case closest ovs of -- the addition of 0.001 * g is because floating numbers will otherwise sometimes round wrong which will make the rays hit their own objects
                                         Just (nObj, a) -> if vecLen (a - origin) < vecLen (p - origin) then Just (nObj, a + scalarMul 0.001 (gradient object a)) else Just (object, p + scalarMul 0.001 (gradient object p))
                                         Nothing        -> Just (object, p + scalarMul 0.001 (gradient object p))

-- Given an object, the point of intersection with that object and the ray that intersected the object,
-- create a new ray with an origin at the bounce and appropriate new direction
bouncedRay :: (Random a, Floating a, Ord a) => Object a -> Vec3 a a a -> Ray a -> R (Ray a)
bouncedRay s@(Sphere _ _ (Lambertian (Vec3 (matR, matG, matB)))) p r@(Ray _ _ rC@(Vec3 (rayR, rayG, rayB))) = do
  dirNoise <- randVecInUnitSphere
  let newDir = unitVector dirNoise
      colMix = scalarMul 0.5 $ scalarMul (vecLen (g+newDir)) (Vec3 (matR*rayR, matG*rayG, matB*rayB)) -- Weighted with the direction of the ray and assuming half of the light is always lost.
  return (Ray newDir p colMix)
  -- All colours are in the interval [0,1].
  -- This means that if the material colour is 1 all of the rays colour is kept, if it's less than one some of it is lost.
  where g = gradient s p
bouncedRay s@(Sphere _ _ Specular) p (Ray rDir _ c) = do -- TODO: Implement different colours
  let g = gradient s p
      d = unitVector rDir
      dirOut = d - scalarMul (2 * dot d g)  g
  return (Ray dirOut p (scalarMul 0.8 c))
-- bouncedRay s@(Sphere _ _ (Refractive i)) p (Ray rDir _ c) = do
--   let g = gradient s p
--       d = unitVector rDir


-- Takes the list of objects, list of rays, ray bounces, rays per pixel. Returns colours
-- TODO: Make into one function
colours :: (Random a, Ord a, Floating a) => [Object a] -> [Ray a] -> Int -> Int-> R [Vec3 a a a]
colours o r bounces rpp = do
  eles <- replicateM rpp (mapM (\x -> cases o 0 x bounces) r)
  return $ map (scalarMul m . sum) $ transpose eles
  where m = 1.0/fromIntegral rpp

cases :: (Random a, Ord a, Floating a) => [Object a] -> Int -> Ray a -> Int -> R (Vec3 a a a)
cases o count currR maxBounce | count > maxBounce = return (Vec3 (0,0,0))
cases o count currR maxBounce = case collideObjects o currR of
                              Nothing -> return (Vec3 (0,0,0))
                              Just (Sphere _ _ (Lightsource _),_) -> return (rayCol currR)
                              Just (obj, p) -> do
                                tmpR <- bouncedRay obj p currR
                                cases o (count+1) tmpR maxBounce
rayCol (Ray _ _ c) = c
