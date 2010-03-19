{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
  import System.Environment (getArgs)
  import Metropolis
  import Sampled
  
  main = do
    [n] <- fmap (map read) getArgs
    let seed = 42
        ev = expectationValue (\(x,y) -> sqrt $ x^2 + y^2) . take n $ metropolis Gauss2DPolar seed
    print ev
  
  data Gauss2DCartesian = Gauss2DCartesian
  instance MetropolisDistribution Gauss2DCartesian (Double,Double) where
    constructSampleWithImportance _ ((u1:u2:_):_) = Just $ value `withImportance` importance where
      value = (x,y)
      (x,y) = (2*u1-1,2*u2-1)
      importance = exp . negate . (30*) $ x^2 + y^2

  data Gauss2DPolar = Gauss2DPolar
  instance MetropolisDistribution Gauss2DPolar (Double,Double) where
    constructSampleWithImportance _ ((u1:u2:_):_) = Just $ value `withImportance` importance where
      importance = contribution * absjacdet
      contribution = exp . negate . (30*) $ r^2
      value = (r*(cos phi),r*(sin phi))
      absjacdet = r  -- dA = r*dr*dphi
      r   = u1
      phi = 2*pi*u2

  -- concatMap ((\(w,(x,y))->printf "{%f,{%f,%f}}," w x y)) . take 10000 . metropolis Gauss2DCartesian $ 46 :: String
