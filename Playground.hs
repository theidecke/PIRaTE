module Playground where

  import UCStream
  
  class MetropolisDistribution a where
    -- | construct a sample from an infinite streams of infinite random numbers ∈ [0,1]
    constructSample :: [Stream] -> (Maybe a)
    -- | probability density in sample-space which we want to sample
    contribution :: a -> Double
    -- | jacobi determinant of the mapping S from the unitcube to sample-space
    -- | which equals the probability density of sample z=S(u) where u ∈ S
    probability :: a -> Double
    -- | transformed contribution in unitspace
    importance :: a -> Double
    
    importance s = (contribution s) / (probability s)


  