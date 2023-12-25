{-# HLINT ignore "Use map" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module LightRay
  ( coloursPar,
    Ray (Ray),
  )
where

import Control.Monad.State.Lazy (replicateM)
import qualified Control.Parallel.Strategies as P
import Data.List (foldl1')
import Data.Maybe (isJust)
import Helpers
import Shapes
import Vector3
import View

{-
TODO: TODO: TODO: TODO: TODO:
See if collide is improvable. About 95% of the time is spent in the collide
function, it would be terrific if i could optimize it.
Rewrite collideObjects using folds.
Rewrite rand. Is about 10% of time spent
-}

data Ray = Ray {dir :: Vec3 , origin :: Vec3 , col :: Vec3 }
  deriving (Show, Eq)

type CollideInfo = (Vec3 , Vec3)

smallNumber :: Double
smallNumber = 0.001

collide :: Object -> Ray -> Maybe CollideInfo
collide s@(Sphere r m _) (Ray v o _)
  -- the checks for K being positive checks if the lightrays actually travel in the
  -- vectors direction, if its negative they travel backwards
  -- TODO: Move gradients somewhere else.
  | disc < 0 = Nothing
  | k1 > 0 && k2 > 0 =
      if sqVecLen d1 < sqVecLen d2
        then Just (p1, gradient s p1)
        else Just (p2, gradient s p2)
  | k1 > 0 = Just (p1, gradient s p1)
  | k2 > 0 = Just (p2, gradient s p2)
  | otherwise = Nothing
  where
    -- Magic numbers. Solving the abc formula. NOTE: Not really abc formula, simplified it a bit
    t = o - m
    a = sqVecLen v
    b = dot v t -- really b/2
    c = sqVecLen t - r * r
    disc = b * b - a * c
    discSqrt = sqrt disc
    k1 = (-b + discSqrt) / a
    k2 = (-b - discSqrt) / a
    d1 = scalarMul k1 v
    d2 = scalarMul k2 v
    p1 = d1 + o
    p2 = d2 + o

gradient :: Object -> Vec3 -> Vec3
gradient (Sphere _ c _) p = unitVector $ p - c
{-# INLINE gradient #-}

-- Given a list of objects and a ray, checks which object is the closest to the rays origin.
-- TODO: Make the scalarMul 0.001 be somewhere else, it gets weird for glass
-- TODO: rewrite closest using folds
collideObjects :: [Object] -> Ray -> Maybe (Object, CollideInfo)
collideObjects o r@(Ray _ origin _) =
  closest $
    (filter (isJust . snd) . zip o) $
      map (`collide` r) o
  where
    closest ((_, Nothing) : ovs) = closest ovs
    closest ((object, Just (point, surfaceNormal)) : ovs) = case closest ovs of
      -- the addition of smallNumber * g is because floating numbers will otherwise
      -- it sometimes round wrong which will make the rays hit their own objects
      Just (prevObj, (prevP, prevSN)) ->
        -- NOTE: store the sqLength of (a-origin)? Should result in speedup
        if sqVecLen (prevP - origin) < sqVecLen (point - origin)
          then Just (prevObj, (prevP, prevSN)) -- if we already got smth then stuffs already added
          else Just (object, (point + scalarMul smallNumber surfaceNormal, surfaceNormal))
      Nothing -> Just (object, (point + scalarMul smallNumber surfaceNormal, surfaceNormal))
    closest [] = Nothing

-- Given an object, the point of intersection with that object and the ray that intersected the object,
-- create a new ray with an origin at the bounce and appropriate new direction
bouncedRay :: Object -> CollideInfo -> Ray -> R Ray
bouncedRay (Sphere _ _ (Lambertian (Vec3 matR matG matB))) (p, g) (Ray _ _ (Vec3 rayR rayG rayB)) = do
  t <- randUnitVec
  -- Make sure that the new dir is not turning > 90 deg in any direction
  -- I thought this would be roughly equivalent to "if dot t g > 0 then t else negate t" but appearantly not
  -- its faster too
  let newDir = unitVector $ g + t
      colMix = scalarMul (dot g newDir) (Vec3 (matR * rayR) (matG * rayG) (matB * rayB))
  return (Ray newDir p colMix)
-- All colours are in the interval [0,1].
-- This means that if the material colour is 1 all of the rays colour is kept, if it's less than one some of it is lost.
bouncedRay (Sphere _ _ (Specular a b)) (p, g) (Ray rDir _ c) = do
  -- TODO: Implement different colours
  t <- randVecInUnitSphere
  let dirOut = reflect rDir g
      f = min b 1
  return (Ray (unitVector $ dirOut + scalarMul f t) p (a * c))
-- TODO: Rewrite this part. For instance, scalarMul 0.0035 norm should be resolved somewhere else
bouncedRay (Sphere _ _ (Refractive i)) (p, g) (Ray rDir _ col) = do
  refMaybe <- rand -- Interval [0,1], deciding if reflecting or not. See schlick's approx wikipedia.
  let isInc = dot g rDir < 0 -- This checks if the ray is incoming towards the object
  -- aka, if going from air to mat or mat to air
  -- if so the dot of g and rdir will be negative
      rri = if isInc then 1 / i else i -- ratio of refractive indices
      norm = if isInc then g else negate g
      cosTheta = -dot rDir norm
      sinTheta = sqrt (1 - cosTheta ^ (2 :: Int))
  return
    ( Ray
        ( if rri * sinTheta > 1.0 || refMaybe < schlicks cosTheta rri
            then reflect rDir norm
            else refract rDir norm rri
        )
        (p - scalarMul (4 * smallNumber) norm)
        col
    )
  where
    schlicks cosTheta rri = r0 + (1 - r0) * (1 - cosTheta) ^ (5 :: Int)
      where
        r0 = ((1 - rri) / (1 + rri)) ^ (2 :: Int)

-- NOTE: Observe that reflect and refract functions both expect all vectors to be unitvectors.
reflect :: Vec3 -> Vec3 -> Vec3
reflect rIn norm = rIn - scalarMul (2 * dot rIn norm) norm

-- Observe that norm is the normal vector such that the dot product of it and rin is positive.
-- Refracts gives the refracted ray given an incoming ray, a normal and ref indices
refract :: Vec3 -> Vec3 -> Double -> Vec3
refract rIn norm rri = rPerpendicular + rParallell
  where
    rPerpendicular = scalarMul rri (rIn + scalarMul (dot (negate rIn) norm) norm)
    rParallell = scalarMul (-sqrt (1 - sqVecLen rPerpendicular)) norm

-- Changes ray to account for defocus blur (aperture size) and antialiasing
changeR :: Camera -> Ray -> R Ray
changeR (CameraInternal {apertureRadius = aR, pixelHeight = pxH, pixelWidth = pxW, vy = vecy, vx = vecx}) (Ray rDir origin col) = do
  xWeight <- rand
  yWeight <- rand
  (ax, ay) <- randInUnitDisc
  let dd = scalarMul ((yWeight - 0.5) * pxH) vecy + scalarMul ((xWeight - 0.5) * pxW) vecx
      pxd = scalarMul (ax * aR) vecx
      pzd = scalarMul (ay * aR) vecy
  return (Ray (unitVector $ rDir + dd) (origin + pxd + pzd) col)

colour :: Camera -> [Object] -> Int -> Int -> Ray -> R Vec3
colour cam o bounces rpp r = do
  eles <-
    replicateM
      rpp
      ( do
          r' <- changeR cam r
          cases o 0 r' bounces
      )
  let t = foldl1' (+) eles
  return $ vSqrt $ (scalarDiv $ fromIntegral rpp) t
  where
    vSqrt (Vec3 a b c) = Vec3 (sqrt a) (sqrt b) (sqrt c)

-- TODO: Would be prettier to have this be in the random monad, and save the runrandom for later.
-- Would probably be more efficient too
coloursPar ::
  Camera ->
  [Object] ->
  Int ->
  Int ->
  [Int] ->
  [Ray] ->
  [Vec3]
coloursPar cam o bounces rpp seeds rs =
  fmap
    (\(x, y) -> runRandom (colour cam o bounces rpp x) y)
    (zip rs seeds)
    `P.using` P.parListChunk 256 P.rseq

cases :: [Object] -> Int -> Ray -> Int -> R Vec3
cases o count currR maxBounce
  | count > maxBounce = return (Vec3 0 0 0)
  | otherwise = case collideObjects o currR of
      Nothing -> return (Vec3 0 0 0)
      Just (Sphere _ _ (Lightsource lc), _) -> return (lc *rayCol currR)
      Just (obj, p) -> do
        tmpR <- bouncedRay obj p currR
        cases o (count + 1) tmpR maxBounce
  where
    rayCol (Ray _ _ c) = c
