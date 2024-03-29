{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use map" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module LightRay
  ( Ray (Ray),
    colours,
  )
where

import BoundingBoxes hiding (singleton)
import Control.Monad.State.Lazy (replicateM)
import Data.List (foldl1')
import Helpers
import Shapes
import Vector3
import View

data Ray = Ray
  { dir :: {-# UNPACK #-} !Vec3,
    origin :: {-# UNPACK #-} !Vec3,
    col :: {-# UNPACK #-} !Vec3
  }
  deriving (Show, Eq)

type CollideInfo = (Vec3, Vec3)

delta :: Double
delta = 0.001

-- times need to overlap for a hit
-- aka (tx0, tx1) and (ty0, ty1) and (tz0, tz1) need to overlap
-- TODO: rewrite prettier
-- also see if it is possible to optimize
collideBox :: BoundingBox -> Ray -> Maybe (Object, CollideInfo)
collideBox b r@(Ray d o _) =
  if overlap (tx0, tx1) (ty0, ty1)
    && overlap (tx0, tx1) (tz0, tz1)
    && overlap (ty0, ty1) (tz0, tz1)
    then case b of
      Node s _ _ -> collide s r >>= (return . (,) s)
      Branch b1 b2 _ _ ->
        case collideBox b1 r of
          Nothing -> collideBox b2 r
          Just c1@(_, (p1, _)) ->
            case collideBox b2 r of
              Nothing -> Just c1
              Just c2@(_, (p2, _)) -> if sqVecLen (p1 - o) < sqVecLen (p2 - o) then Just c1 else Just c2
    else Nothing
  where
    (lc, hc) = extractLCHC b
    -- easier if we make sure the vecs are ordered
    (Vec3 (tx0, ty0, tz0)) = vecZipWith min t t'
    (Vec3 (tx1, ty1, tz1)) = vecZipWith max t t'
    t = (lc - o) * elemInverse d -- (*) is elementwise multiplication (Hadamard product)
    t' = (hc - o) * elemInverse d
    overlap (a, b) (c, d) =
      (a > c && a < d)
        || (b > c && b < d)
        || (a < c && b > d) -- since the pairs are ordered, there can't be a case where b < c && a > d

collide :: Object -> Ray -> Maybe CollideInfo
collide s@(Sphere r m _) (Ray d o _)
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
    a = sqVecLen d
    b = dot d t -- really b/2
    c = sqVecLen t - r * r
    disc = b * b - a * c
    discSqrt = sqrt disc
    k1 = (-b + discSqrt) / a
    k2 = (-b - discSqrt) / a
    d1 = scalarMul k1 d
    d2 = scalarMul k2 d
    p1 = d1 + o
    p2 = d2 + o

gradient :: Object -> Vec3 -> Vec3
gradient (Sphere _ c _) p = unitVector $ p - c
{-# INLINE gradient #-}

-- Given an object, the point of intersection with that object and the ray that intersected the object,
-- create a new ray with an origin at the bounce and appropriate new direction
bouncedRay :: Object -> CollideInfo -> Ray -> R Ray
bouncedRay (Sphere _ _ (Lambertian matC)) (p, g) (Ray _ _ rayC) = do
  t <- randUnitVec
  -- Make sure that the new dir is not turning > 90 deg in any direction
  -- I thought this would be roughly equivalent to "if dot t g > 0 then t else negate t" but appearantly not
  -- its faster too
  let newDir = unitVector $ g + t
      colMix = scalarMul (dot g newDir) (matC * rayC)
  return (Ray newDir p colMix)
-- All colours are in the interval [0,1].
-- This means that if the material colour is 1 all of the rays colour is kept, if it's less than one some of it is lost.
bouncedRay (Sphere _ _ (Specular a b)) (p, g) (Ray rDir _ c) = do
  -- TODO: Implement different colours
  t <- randVecInUnitSphere
  let dirOut = reflect rDir g
      f = min b 1
  return (Ray (unitVector $ dirOut + scalarMul f t) p (a * c))
-- IMPORTANT: Refractive (most likely) is bugged, it looks weird
bouncedRay (Sphere _ _ (Refractive i)) (p, g) (Ray rDir _ col) = do
  refMaybe <- rand -- Interval [0,1], deciding if reflecting or not. See schlick's approx wikipedia.
  let isInc = dot g rDir < 0 -- This checks if the ray is incoming towards the object
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
        (p - scalarMul delta norm)
        col
    )
  where
    schlicks cosTheta rri = r0 + (1 - r0) * (1 - cosTheta) ^ (5 :: Int)
      where
        r0 = ((1 - rri) / (1 + rri)) ^ (2 :: Int)

-- NOTE: Observe that reflect and refract functions both expect all vectors to be unitvectors.
reflect :: Vec3 -> Vec3 -> Vec3
reflect rIn norm = rIn - scalarMul (2 * dot rIn norm) norm

-- Observe that norm is the normal vector such that the dot product of it and rin is negative.
-- Refracts gives the refracted ray given an incoming ray, a normal and ref indices
refract :: Vec3 -> Vec3 -> Double -> Vec3
refract rIn norm rri = rPerpendicular + rParallell
  where
    rPerpendicular = scalarMul rri (rIn + scalarMul (dot (-rIn) norm) norm)
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

colour :: Camera -> BoundingBox -> Int -> Int -> Ray -> R Vec3
colour cam b bounces rpp r = do
  eles <-
    replicateM
      rpp
      ( do
          r' <- changeR cam r
          cases b 0 r' bounces
      )
  let t = foldl1' (+) eles
  return $ vSqrt $ (scalarDiv $ fromIntegral rpp) t
  where
    vSqrt = vecMap sqrt

colours :: (Traversable t) => 
  Camera ->
  BoundingBox ->
  Int ->
  Int ->
  t Ray ->
  R (t Vec3)
colours cam b bounces rpp = mapM (colour cam b bounces rpp)

cases :: BoundingBox -> Int -> Ray -> Int -> R Vec3
cases b count currR maxBounce
  | count > maxBounce = return 0
  | otherwise = case collideBox b currR of
      Nothing -> return 0
      Just (Sphere _ _ (Lightsource lc), _) -> return (lc * rayCol currR)
      Just (obj, p) -> do
        tmpR <- bouncedRay obj p currR
        cases b (count + 1) tmpR maxBounce
  where
    rayCol (Ray _ _ c) = c
