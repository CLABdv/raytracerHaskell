module BoundingBoxes
  ( constructor,
    BoundingBox (BoundingBox),
    ObjectTree (Node, Branch),
    singleton,
    getLowCorner,
    getHighCorner,
  )
where

import Data.List (sortBy)
import Data.Word (Word32)
import Helpers
import Shapes
import Vector3

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
  return $ BoundingBox (Branch (extractContent box1) (extractContent box2)) (getLowCorner box1 box2) (getHighCorner box1 box2)

sortx :: [BoundingBox] -> [BoundingBox]
sortx =
  sortBy
    ( \(BoundingBox _ (Vec3 lx1 _ _) (Vec3 hx1 _ _))
       (BoundingBox _ (Vec3 lx2 _ _) (Vec3 hx2 _ _)) -> compare ((lx1 + hx1) / 2) ((lx2 + hx2) / 2)
    )

sorty :: [BoundingBox] -> [BoundingBox]
sorty =
  sortBy
    ( \(BoundingBox _ (Vec3 _ ly1 _) (Vec3 _ hy1 _))
       (BoundingBox _ (Vec3 _ ly2 _) (Vec3 _ hy2 _)) -> compare ((ly1 + hy1) / 2) ((ly2 + hy2) / 2)
    )

sortz :: [BoundingBox] -> [BoundingBox]
sortz =
  sortBy
    ( \(BoundingBox _ (Vec3 _ _ lz1) (Vec3 _ _ hz1))
       (BoundingBox _ (Vec3 _ _ lz2) (Vec3 _ _ hz2)) -> compare ((lz1 + hz1) / 2) ((lz2 + hz2) / 2)
    )

getLowCorner :: BoundingBox -> BoundingBox -> Vec3
getLowCorner (BoundingBox _ (Vec3 x1 y1 z1) _) (BoundingBox _ (Vec3 x2 y2 z2) _) = Vec3 (min x1 x2) (min y1 y2) (min z1 z2)

getHighCorner :: BoundingBox -> BoundingBox -> Vec3
getHighCorner (BoundingBox _ (Vec3 x1 y1 z1) _) (BoundingBox _ (Vec3 x2 y2 z2) _) = Vec3 (max x1 x2) (max y1 y2) (max z1 z2)
