{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map" #-}
module LightRay (module LightRay) where

import Control.Monad.State.Lazy
import Data.List (foldl', foldl1', transpose)
import Data.Maybe (isJust, isNothing)
import Helpers
import Shapes
import System.Random (Random)
import Vector3

{-
TODO:
stuff
-}

data Ray a = Ray {dir :: Vec3 a, origin :: Vec3 a, col :: Vec3 a}
  deriving (Show, Eq)

-- Checks for collision with object. Applies equation of a sphere and gets intersections with abc formula
collide :: (Ord a, Floating a) => Object a -> Ray a -> Maybe (Vec3 a)
collide (Sphere r (Vec3 mx my mz) _) (Ray v@(Vec3 vx vy vz) o@(Vec3 ox oy oz) _)
  -- the checks for K being positive checks if the lightrays actually travel in the
  -- vectors direction, if its negative they travel backwards
  | det < 0 = Nothing
  | k1 >= 0 && k2 >= 0 =
      if vecLen d1 < vecLen d2
        then Just (d1 + o)
        else Just (d2 + o)
  | k1 >= 0 = Just (d1 + o)
  | k2 >= 0 = Just (d2 + o)
  | otherwise = Nothing
  where
    -- Magic numbers. Solving the abc formula.
    a = vx ^ 2 + vy ^ 2 + vz ^ 2
    b = 2 * vx * (ox - mx) + 2 * vy * (oy - my) + 2 * vz * (oz - mz)
    c = (sum . map (^ 2) $ zipWith (-) [mx, my, mz] [ox, oy, oz]) - r ^ 2
    det = b ^ 2 - 4 * a * c
    detSqrt = sqrt det
    k1 = (-b + detSqrt) / (2 * a)
    k2 = (-b - detSqrt) / (2 * a)
    d1 = scalarMul k1 v
    d2 = scalarMul k2 v

unsafeCollide x y = case collide x y of
  Just a -> a
  Nothing -> error "Unsafe collide did not succeed"

-- Given an object and its point of intersection, give the normal vector of the tangent plane
-- NOTE: Maybe remove unitvector from this function since that is not always desired.
gradient :: Floating a => Object a -> Vec3 a -> Vec3 a
gradient (Sphere _ c _) p = unitVector $ p - c

-- Given a list of objects and a ray, checks which object is the closest to the rays origin.
-- TODO: Make the scalarMul 0.001 be somewhere else, it doesnt work for glass
collideObjects :: (Ord a, Floating a) => [Object a] -> Ray a -> Maybe (Object a, Vec3 a)
collideObjects o r@(Ray _ origin _) = closest $ (filter (isJust . snd) . zip o) $ map (`collide` r) o
  where
    -- closest' obs = foldl1' (\n acc)
    closest [] = Nothing
    closest ((_, Nothing) : ovs) = closest ovs
    closest ((object, Just p) : ovs) = case closest ovs of -- the addition of 0.001 * g is because floating numbers will otherwise sometimes round wrong which will make the rays hit their own objects
      Just (nObj, a) ->
        if vecLen (a - origin) < vecLen (p - origin)
          then Just (nObj, a + scalarMul 0.001 (gradient object a))
          else Just (object, p + scalarMul 0.001 (gradient object p))
      Nothing -> Just (object, p + scalarMul 0.001 (gradient object p))

-- Given an object, the point of intersection with that object and the ray that intersected the object,
-- create a new ray with an origin at the bounce and appropriate new direction
bouncedRay :: (Random a, Floating a, Ord a) => Object a -> Vec3 a -> Ray a -> R (Ray a)
bouncedRay s@(Sphere _ _ (Lambertian (Vec3 matR matG matB))) p r@(Ray _ _ rC@(Vec3 rayR rayG rayB)) = do
  dirNoise <- randVecInUnitSphere
  let newDir = unitVector dirNoise
      colMix = scalarDiv 2 $ scalarMul (vecLen (g + newDir)) (Vec3 (matR * rayR) (matG * rayG) (matB * rayB)) -- Weighted with the direction of the ray and assuming half of the light is always lost.
  return (Ray newDir p colMix)
  where
    -- All colours are in the interval [0,1].
    -- This means that if the material colour is 1 all of the rays colour is kept, if it's less than one some of it is lost.
    g = gradient s p
bouncedRay s@(Sphere _ _ Specular) p (Ray rDir _ c) = do
  -- TODO: Implement different colours
  let g = gradient s p
      d = unitVector rDir
      dirOut = reflect d g
  return (Ray dirOut p (scalarMul 0.8 c))
-- TODO: Rewrite this part. For instance, scalarMul 0.002 norm should be resolved somewhere else
bouncedRay s@(Sphere _ _ (Refractive i)) p (Ray rDir _ col) = do
  refMaybe <- rand -- Interval [0,1], deciding if reflecting or not. See schlick's approx wikipedia.
  let gsp = gradient s p
      isInc = dot gsp rDir < 0 -- This checks if the ray is incoming towards the object
      -- aka, if going from air to mat or mat to air
      -- if so the dot of gsp and rdir will be negative
      rri = if isInc then 1 / i else i -- ratio of refractive indices
      unitRDir = unitVector rDir
      norm = if isInc then gsp else negate gsp
      cosTheta = dot (negate unitRDir) norm
      sinTheta = sqrt (1 - cosTheta ^ 2)
  return
    ( Ray
        ( if rri * sinTheta > 1.0 || refMaybe < schlicks cosTheta rri
            then reflect unitRDir norm
            else refract unitRDir norm rri
        )
        (p - scalarMul 0.0035 norm)
        col
    )
  where
    schlicks cosTheta rri = r0 + (1 - r0) * (1 - cosTheta) ^ 5
      where
        r0 = ((1 - rri) / (1 + rri)) ^ 2

-- NOTE: Observe that reflect and refract functions both expect all vectors to be unitvectors.
reflect :: Num a => Vec3 a -> Vec3 a -> Vec3 a
reflect rIn norm = rIn - scalarMul (2 * dot rIn norm) norm

-- Observe that norm is the normal vector such that the dot product of it and rin is positive.
-- Refracts gives the refracted ray given an incoming ray, a normal and ref indices
refract :: Floating p => Vec3 p -> Vec3 p -> p -> Vec3 p
refract rIn norm rri = rPerpendicular + rParallell
  where
    rPerpendicular = scalarMul rri (rIn + scalarMul (dot (negate rIn) norm) norm)
    rParallell = scalarMul (-sqrt (1 - vecLen rPerpendicular ^ 2)) norm

colours :: (Random a, Ord a, Floating a) => [Object a] -> [Ray a] -> Int -> Int -> R [Vec3 a]
colours o r bounces rpp = do
  eles <- replicateM rpp (mapM (\x -> cases o 0 x bounces) r)
  let t = foldl1' (zipWith (+)) eles
  return $ map (scalarDiv $ fromIntegral rpp) t

cases :: (Random a, Ord a, Floating a, Integral b) => [Object a] -> b -> Ray a -> b -> R (Vec3 a)
cases o count currR maxBounce
  | count > maxBounce = return (Vec3 0 0 0)
  | otherwise = case collideObjects o currR of
      Nothing -> return (Vec3 0 0 0)
      Just (Sphere _ _ (Lightsource _), _) -> return (rayCol currR)
      -- rewrite???
      Just (obj, p) -> do
        tmpR <- bouncedRay obj p currR
        cases o (count + 1) tmpR maxBounce
  where
    rayCol (Ray _ _ c) = c
