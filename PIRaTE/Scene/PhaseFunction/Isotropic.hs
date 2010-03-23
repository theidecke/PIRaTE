{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module PIRaTE.Scene.PhaseFunction.Isotropic where
  import Data.ACVector (Vector3(..))
  import PIRaTE.SpatialTypes
  import PIRaTE.MonteCarlo.Sampled
  
  -- Isotropic PhaseFunction
  data Isotropic = Isotropic
                     
  instance Show Isotropic where
    show Isotropic = "Isotropic"

  instance Sampleable (Isotropic,Ray) Direction where
    sampleProbabilityOf (Isotropic,_) d = 1/(4*pi)
    {-# INLINE sampleProbabilityOf #-}
    
    sampleFrom (Isotropic,_) = do
      dir <- randomIsotropicDirection
      return . Just $ dir `withImportance` (4*pi)
    {-# INLINE sampleFrom #-}
    
  -- generates a random direction vector with length one
  randomIsotropicDirection :: UCStreamTo Direction
  randomIsotropicDirection = do
    u1 <- getCoord
    u2 <- getCoord
    let z = 2*(u1::Double) - 1
        phi = 2*pi*(u2::Double)
        rho = sqrt (1 - z*z)
    return . Direction $ Vector3 (rho * cos phi) (rho * sin phi) z
  {-# INLINE randomIsotropicDirection #-}

  
  {--
  -- generates a normalized Direction-Vector that is pointing towards a point inside a cone pointing in the positive z-axis with solid angle 2*pi*capheight, capheight = 1 - cos theta
  randomDirectionInZCone :: Double -> Gen s -> ST s Direction
  randomDirectionInZCone capheight g = do
      u1 <- uniform g
      u2 <- uniform g
      let oneminusz = capheight*(u1::Double)  --capheight = 1 - costheta
          phi = 2*pi*(u2::Double)
          rho = sqrt (oneminusz*(2-oneminusz))--sqrt (1 - z*z)
          z = 1 - oneminusz
      return . Direction $ Vector3 (rho * cos phi) (rho * sin phi) z
  --}