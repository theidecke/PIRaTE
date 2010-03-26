{-# LANGUAGE MultiParamTypeClasses #-}

module PoissonDistribution (
    poissonDistFromLambda,
    sampleWithImportanceFrom,
    poissonPDF
  ) where

  import PIRaTE.MonteCarlo.Sampled
  import PIRaTE.MonteCarlo.UCStream (UCStreamTo,getCoord)
  
  newtype PoissonDistribution = PoissonDistribution Double
  poissonDistFromLambda l = PoissonDistribution l

  -- | implemented simple poisson sampling algorithm from:
  -- | Donald E. Knuth: Seminumerical Algorithms. The Art of Computer Programming, Volume 2
  instance Sampleable PoissonDistribution Integer where
    sampleProbabilityOf (PoissonDistribution lambda) k = poissonPDF lambda k
    sampleWithImportanceFrom (PoissonDistribution lambda) = do
      poissonsample <- poissonStep (exp (-lambda)) 0 1
      return . Just $ poissonsample `withProbability` poissonPDF lambda poissonsample
  
  poissonStep :: Double -> Integer -> Double -> UCStreamTo Integer
  poissonStep l k p = do
    u <- getCoord
    let p' = p * u
    if p' > l
      then poissonStep l (k+1) p'
      else return k
  
  poissonPDF lambda k = exp $ (log lambda)*k' - (lnGamma (k'+1)) - lambda where
    k' = fromIntegral k
  
  -- | lanczos approximation for the natural logarithm of Gamma(z)
  -- | adapted from the GSL-version
  lnGamma zp1 = logsqrt2pi + (z+0.5)*(log t) - t + (log x)  where
    logsqrt2pi = 0.9189385332046727
    t = z + g + 0.5
    x = fst $ foldl step (p0,1::Int) ps
    step (psum,i) p = (psum + p/(z + (fromIntegral i)), i+1)
    z = zp1 - 1
    ps = [676.5203681218851, -1259.1392167224028, 771.32342877765313, 
          -176.61502916214059, 12.507343278686905, -0.13857109526572012,
          9.9843695780195716e-6, 1.5056327351493116e-7]
    p0 = 0.99999999999980993
    g = 7