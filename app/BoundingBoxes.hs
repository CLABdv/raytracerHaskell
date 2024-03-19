module BoundingBoxes
  ( constructor,
    BoundingBox (Branch, Node),
    singleton,
    getLowCorner,
    getHighCorner,
    extractLCHC,
  )
where

import Data.List (sortBy)
import Data.Word (Word32)
import Helpers
import Shapes
import Vector3
import Data.Ord (comparing)

data BoundingBox
  = Branch
      { c1 :: BoundingBox,
        c2 :: BoundingBox,
        lc :: {-# UNPACK #-} !Vec3, -- low corner, all low values
        hc :: {-# UNPACK #-} !Vec3 -- high corner, all high values
      }
  | Node
      { c :: Object,
        lc :: {-# UNPACK #-} !Vec3,
        hc :: {-# UNPACK #-} !Vec3
      }
  deriving (Eq, Show)

singleton :: Object -> BoundingBox
singleton s@(Sphere r m _) = let rVec = Vec3 r r r in Node s (m - rVec) (m + rVec)

constructor :: [Object] -> R BoundingBox
constructor = _constructor . map singleton

_constructor :: [BoundingBox] -> R BoundingBox
_constructor [o] = return o -- singleton
_constructor os = do
  axis <- (rand >>= (return . (`mod` 3))) :: R Word32
  let sf = case axis of
        0 -> sortx
        1 -> sorty
        2 -> sortz
        _ -> error "a Word32 mod 3 returned something not in [0,2]"
      (a, b) = splitAt (length os `div` 2) $ sf os
  box1 <- _constructor a
  box2 <- _constructor b
  return $ Branch box1 box2 (getLowCorner box1 box2) (getHighCorner box1 box2)

extractLCHC :: BoundingBox -> (Vec3, Vec3)
extractLCHC (Branch _ _ lc hc) = (lc, hc)
extractLCHC (Node _ lc hc) = (lc, hc)

sortx :: [BoundingBox] -> [BoundingBox]
sortx = sortBy (comparing xCenter)
    where xCenter v = let (Vec3 lx _ _, Vec3 hx _ _) = extractLCHC v in (hx + lx) / 2
 
sorty :: [BoundingBox] -> [BoundingBox]
sorty = sortBy (comparing yCenter)
    where yCenter v = let (Vec3 _ ly _, Vec3 _ hy _) = extractLCHC v in (hy + ly) / 2
    
sortz :: [BoundingBox] -> [BoundingBox]
sortz = sortBy (comparing zCenter)
    where zCenter v = let (Vec3 _ _ lz, Vec3 _ _ hz) = extractLCHC v in (hz + lz) / 2

getLowCorner :: BoundingBox -> BoundingBox -> Vec3
getLowCorner b1 b2 = vecZipWith min v1 v2
    where (v1, _) = extractLCHC b1
          (v2, _) = extractLCHC b2

getHighCorner :: BoundingBox -> BoundingBox -> Vec3
getHighCorner b1 b2 = vecZipWith max v1 v2
    where (_, v1) = extractLCHC b1
          (_, v2) = extractLCHC b2
