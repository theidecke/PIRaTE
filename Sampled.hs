{-# LANGUAGE MultiParamTypeClasses #-}

module Sampled (
    Sampleable(..),
    UCStreamTo,
    getCoord,
    Sampled,
    sampledValue,
    sampledAbsJacDet,
    withAbsJacDet,
    withProbability,
    andWith
  ) where

  import Control.Monad.State
  import UCStream (UCStreamTo,getCoord)

  class Sampleable a b where
    -- | construct a sampler which yields "b"s from an "a" using a stream of unitcoordinates
    sampleFrom :: a -> UCStreamTo (Sampled b)

  -- | sample z of type a with the absolute jacobian determinant of its transformation from unitcubespace.
  -- | this equals the reciprocal of the probability density |det(dS(u)/du)| = 1/p_S(u), z = S(u)
  type Sampled a = (Double, a)
  
  sampledValue = snd
  sampledAbsJacDet = fst
  withAbsJacDet v ajd = (ajd,v)
  withProbability v p = (1/p,v)
  
  andWith f s1 s2 = (v1 `f` v2) `withAbsJacDet` (ajd1 * ajd2) where
    v1 = sampledValue s1
    v2 = sampledValue s2
    ajd1 = sampledAbsJacDet s1
    ajd2 = sampledAbsJacDet s2
