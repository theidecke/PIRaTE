{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PIRaTE.Scene.Container.Box where
  import Data.ACVector (Vector3(..))
  import PIRaTE.SpatialTypes
  import PIRaTE.Scene.Confineable
  import PIRaTE.MonteCarlo.Sampled
  
  data Box = Box {
      corner1 :: Point,
      corner2 :: Point
    } deriving (Eq)

  fromCorners (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    Box (Vector3 (min x1 x2) (min y1 y2) (min z1 z2))
        (Vector3 (max x1 x2) (max y1 y2) (max z1 z2))
  {-# INLINE fromCorners #-}

  instance Show Box where
    show (Box c1 c2) = "Box spanned between " ++ (showVector3 c1) ++
                       " and " ++ (showVector3 c2)

  instance Confineable Box where
    contains (Box (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) (Vector3 x y z) =
      not $ or [x<x1, x>x2, y<y1, y>y2, z<z1, z>z2]
    {-# INLINE contains #-}
    
    intersectedBy (Box (Vector3 x1 y1 z1) (Vector3 x2 y2 z2))
                  (Ray (Vector3 ox oy oz) (Direction (Vector3 dx dy dz)))
      | tmin > tmax = []
      | otherwise   = [(tmin,tmax)]
      where tmin = maximum [txmin,tymin,tzmin]
            tmax = minimum [txmax,tymax,tzmax]
            (txmin,txmax) | dx >= 0   = ((x1-ox)*dxinv, (x2-ox)*dxinv)
                          | otherwise = ((x2-ox)*dxinv, (x1-ox)*dxinv)
            (tymin,tymax) | dy >= 0   = ((y1-oy)*dyinv, (y2-oy)*dyinv)
                          | otherwise = ((y2-oy)*dyinv, (y1-oy)*dyinv)
            (tzmin,tzmax) | dz >= 0   = ((z1-oz)*dzinv, (z2-oz)*dzinv)
                          | otherwise = ((z2-oz)*dzinv, (z1-oz)*dzinv)
            dxinv = 1 / dx
            dyinv = 1 / dy
            dzinv = 1 / dz


  instance Sampleable Box Point where
    sampleProbabilityOf box p
      | box `contains` p = 1 / (boxVolume box)
      | otherwise        = 0
    {-# INLINE sampleProbabilityOf #-}

    sampleWithImportanceFrom (Box (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) = do
      u1 <- lift getCoord
      u2 <- lift getCoord
      u3 <- lift getCoord
      let w = x2-x1
          d = y2-y1
          h = z2-z1
          x = x1 + w * u1
          y = y1 + d * u2
          z = z1 + h * u3
          boxvolume = w*d*h
      return $ Vector3 x y z `withImportance` boxvolume
    {-# INLINE sampleWithImportanceFrom #-}


  boxVolume (Box (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) =
    (x2-x1)*(y2-y1)*(z2-z1)
  {-# INLINE boxVolume #-}
