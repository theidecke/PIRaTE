{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module PIRaTE.Scene.PhaseFunction.ZCone where
  import Data.ACVector (Vector3(..),v3z)
  import PIRaTE.SpatialTypes
  import PIRaTE.MonteCarlo.Sampled
  
  -- PhaseFunction which scatters all incoming light into a cone around the z axis
  data ZCone = ZCone Double
  
  fromApexAngle :: Double -> ZCone
  fromApexAngle twotheta = ZCone capheight
      where capheight = 1 - cos theta
            theta = 0.5 * twotheta
  
  fromCapHeight :: Double -> ZCone
  fromCapHeight capheight = ZCone capheight

  instance Show ZCone where
    show (ZCone h) = "ZCone with capheight " ++ (show h)

  {--instance Sampleable (ZCone,Ray) Direction where
    sampleProbabilityOf (ZCone h,_) (Direction wout)
      | v3z wout > 1 - h = isotropicprob / areacoverage
      | otherwise        = 0
      where areacoverage = 0.5*h
            isotropicprob = 1 / (4*pi)
    {-# INLINE sampleProbabilityOf #-}
    randomSampleFrom     (ZCone h,_) g = randomDirectionInZCone h g
    {-# INLINE randomSampleFrom #-}--}

  instance Sampleable (ZCone,Ray) Direction where
    sampleProbabilityOf (ZCone h,_) (Direction wout)
      | v3z wout > 1 - h = isotropicprob / areacoverage
      | otherwise        = 0
      where areacoverage = 0.5*h
            isotropicprob = 1 / (4*pi)
    {-# INLINE sampleProbabilityOf #-}

    sampleWithImportanceFrom (ZCone h,_) = do
      dir <- lift . randomDirectionInZCone $ h
      let areacoverage = 0.5*h
          isotropicjacobian = 4*pi
      return $ dir `withImportance` (areacoverage*isotropicjacobian)
    {-# INLINE sampleWithImportanceFrom #-}

  -- generates a normalized Direction-Vector that is pointing towards a point inside a cone pointing in the positive z-axis with solid angle 2*pi*capheight, capheight = 1 - cos theta
  randomDirectionInZCone :: Double -> UCStreamTo Direction
  randomDirectionInZCone capheight = do
      u1 <- getCoord
      u2 <- getCoord
      let oneminusz = capheight*(u1::Double)  --capheight = 1 - costheta
          phi = 2*pi*(u2::Double)
          rho = sqrt (oneminusz*(2-oneminusz))--sqrt (1 - z*z)
          z = 1 - oneminusz
      return . Direction $ Vector3 (rho * cos phi) (rho * sin phi) z
  {-# INLINE randomDirectionInZCone #-}
