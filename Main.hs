{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
  import System.Environment (getArgs)
  import Control.Monad.State (evalState)
  import Metropolis
  import Sampled
  import PoissonDistribution
  
  main = do
    [n,seed] <- fmap (map read) getArgs
    let ev = expectationValue (\(x,y) -> sqrt $ x^2 + y^2) . take n $ metropolis Gauss2DPolar (fromIntegral seed)
    print ev
  
  data Gauss2DCartesian = Gauss2DCartesian deriving Show
  instance MetropolisDistribution Gauss2DCartesian (Double,Double) where
    constructSampleWithImportance _ ((u1:u2:_):_) = Just $ value `withImportance` importance where
      value = (x,y)
      (x,y) = (2*u1-1,2*u2-1)
      importance = exp . negate . (12.5*) $ x^2 + y^2

  data Gauss2DPolar = Gauss2DPolar deriving Show
  instance MetropolisDistribution Gauss2DPolar (Double,Double) where
    constructSampleWithImportance _ ((u1:u2:_):_) = Just $ value `withImportance` importance where
      importance = contribution * absjacdet
      contribution = exp . negate . (0.5*) $ (5*r)^2
      value = (r*(cos phi),r*(sin phi))
      absjacdet = r  -- dA = r*dr*dphi
      r   = u1
      phi = 2*pi*u2

  -- concatMap ((\(w,(x,y))->printf "{%f,{%f,%f}}," w x y)) . take 10000 . metropolis Gauss2DCartesian $ 46 :: String
  newtype ConvenientDistribution = ConvenientDistribution Double
  instance Sampleable ConvenientDistribution Integer where
    sampleFrom (ConvenientDistribution a) = do
      x <- getCoord
      let sample = floor $ a*x/(1-x)
          k      = fromIntegral sample
          prob   = a/((a+k)*(a+k+1))
      return $ sample `withProbability` prob
  
  data Poisson = Poisson Double deriving Show
  instance MetropolisDistribution Poisson Integer where
    constructSampleWithImportance (Poisson lambda) (stream:_) = Just $ k `withImportance` importance where
      importance = contribution * invprobability
      contribution = poissonPDF lambda k
      (invprobability,k) = evalState (sampleFrom cdist) stream
      cdist = ConvenientDistribution 4.0
    
  -- expectationValue fromIntegral . take 100000 . metropolis (Poisson 4.0) $ 631
  -- take 100 . map snd . evalState (sequence . repeat $ sampleFrom (ConvenientDistribution 2.0)) . toStream $ 14 :: [Integer]