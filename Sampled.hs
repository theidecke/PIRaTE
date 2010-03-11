{-# LANGUAGE MultiParamTypeClasses #-}

module Sampled where

  import Control.Monad.State

  class Sampleable a b where
    randomSampleFrom    :: a -> State s (Sampled b)

  -- | value of type a with its probability density
  type Sampled a = (Double, a)
  
  sampledValue = snd
  sampledImportance = fst
  toSampled v i = (i,v)
  
  andWith f s1 s2 = toSampled (v1 `f` v2) (i1 * i2) where
    v1 = sampledValue s1
    v2 = sampledValue s2
    i1 = sampledImportance s1
    i2 = sampledImportance s2
