{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.MonteCarlo.Sampled (
    Sampleable(..),
    Sampled,
    sampledValue,
    sampledImportance,
    withImportance,
    withProbability,
    scaleImportanceWith,
    continueSamplingFrom,
    UCStreamTo,
    getCoord
  ) where

  import Control.Monad.State
  import Control.Applicative
  import PIRaTE.MonteCarlo.UCStream (UCStreamTo,getCoord)

  type MaybeSampled b = Maybe (Sampled b)
  type UCToMaybeSampled b = UCStreamTo (MaybeSampled b)

  class Sampleable a b where
    -- | computes the probability density of sampling a particular "b" from an "a"
    sampleProbabilityOf :: a -> b -> Double
    -- | construct a sampler which yields "b"s from an "a" using a stream of unitcoordinates
    sampleFrom :: a -> UCToMaybeSampled b

  -- | sample z of type a with its importance prepended. The importance is the product of
  -- | sample-contribution and the absolute jacobian determinant of its transformation from unitcubespace
  -- | i = c*ajd = c/p
  -- | which equals the reciprocal of the probability density |det(dS(u)/du)| = 1/p_S(u), z = S(u)

  newtype Sampled a = Sampled (Double, a) deriving Show
  
  sampledValue      (Sampled (  _,v)) = v
  sampledImportance (Sampled (ajd,_)) = ajd
  withImportance v ajd = Sampled (ajd,v)
  withProbability v p  = Sampled (1/p,v)
  scaleImportanceWith s icoeff = (sampledValue s) `withImportance` (icoeff * (sampledImportance s))

  continueSamplingFrom :: (Sampleable b c) => (a -> b) -> MaybeSampled a -> UCToMaybeSampled c
  continueSamplingFrom f ms = case ms of
      Nothing -> return Nothing
      Just s  -> do let v = f (sampledValue s)
                        oldi = sampledImportance s
                    ms' <- sampleFrom v
                    case ms' of
                      Nothing -> return Nothing
                      Just s' -> return . Just $ s' `scaleImportanceWith` oldi
    

  andWith f s1 s2 = (v1 `f` v2) `withImportance` (i1 * i2) where
    v1 = sampledValue s1
    v2 = sampledValue s2
    i1 = sampledImportance s1
    i2 = sampledImportance s2
