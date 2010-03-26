{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PIRaTE.Scene.Container.Sphere where
  import Data.ACVector (Vector3(..),(|*),vdot)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq)
  import PIRaTE.Scene.Confineable
  import PIRaTE.MonteCarlo.Sampled
  
  -- Sphere
  data Sphere = Sphere {
      center :: Point,
      radius :: Double
    } deriving (Eq)

  instance Show Sphere where
    show (Sphere c r) = "Sphere at " ++ (showVector3 c) ++ " with Radius " ++ (show r)

  -- Sphere implements the Confineable type class functions
  instance Confineable Sphere where
    contains (Sphere c r) p = normsq (p - c) <= r*r
    {-# INLINE contains #-}
    
    -- assuming direction is normalized
    intersectedBy (Sphere center radius) (Ray origin (Direction direction)) = 
      let offset = origin - center
          oo = offset `vdot` offset
          od = offset `vdot` direction
          discriminant = od*od - oo + radius*radius
      in if discriminant <= 0 
          then []
          else let alpha0 = -od
                   alphadelta = sqrt discriminant
                   alphas = (alpha0 - alphadelta,
                             alpha0 + alphadelta)
               in [alphas]

  instance Sampleable Sphere Point where
    sampleProbabilityOf s@(Sphere center radius) p
      | s `contains` p = 3/(4*pi * radius^3)
      | otherwise      = 0
    {-# INLINE sampleProbabilityOf #-}

    sampleWithImportanceFrom (Sphere center radius) = do
      unitpoint <- lift randomPointInUnitSphere
      let translation = radius |* unitpoint
          jacobian = 4*pi/3 * radius^3
      return $ (center + translation) `withImportance` jacobian
    {-# INLINE sampleWithImportanceFrom #-}

  -- | samples homogenously distributed points inside the unitsphere
  -- | by sampling z and phi uniformly, determining a point on the spheresurface
  randomPointInUnitSphere = do
    u1 <- getCoord
    u2 <- getCoord
    u3 <- getCoord
    let z   = 2 * u1 - 1
        rho = sqrt $ 1 - z^2
        phi = 2*pi * u2
        x = rho*(cos phi)
        y = rho*(sin phi)
        r   = u3**(1/3)
    return $ r |* Vector3 x y z
  --  concat . intersperse "," . map showVector3 . evalState (replicateM 1000 randomPointInUnitSphere) $ toStream 13
  