{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.MonteCarlo.Sampled (
    Sampleable(..),
    importanceFromCP,
    Sampled,
    sampledValue,
    sampledImportance,
    withImportance,
    withProbability,
    scaleImportanceWith,
    UCStreamTo,
    getCoord,
    UCToMaybeSampled,
    runUCToMaybeSampled,
    MaybeSampled,
    lift
  ) where

  import Control.Monad.State
  import Control.Monad.Maybe
  import PIRaTE.MonteCarlo.UCStream (UCStreamTo,getCoord)

  newtype Sampled a = Sampled (Double, a) deriving Show
  
  sampledValue      (Sampled (  _,v)) = v
  sampledImportance (Sampled (ajd,_)) = ajd
  withImportance v ajd = Sampled (ajd,v)
  withProbability v p  = Sampled (1/p,v)
  scaleImportanceWith (Sampled (i,v)) icoeff =  Sampled (icoeff*i,v)

  type UCToMaybeSampled b = MaybeT UCStreamTo (Sampled b)
  type UCToMaybe b = MaybeT UCStreamTo b
  runUCToMaybeSampled = evalState . runMaybeT
  type MaybeSampled b = Maybe (Sampled b)

  class Sampleable a b where
    -- | construct a sampler which yields "b"s from an "a" using a stream of unitcoordinates
    sampleWithImportanceFrom :: a -> UCToMaybeSampled b
    sampleWithImportanceFrom a = do
      sample <- sampleFrom a
      let importance = sampleImportance a sample
      return $ sample `withImportance` importance

    -- | computes the probability density of sampling a particular "b" from an "a"
    sampleProbabilityOf  :: a -> b -> Double

    -- | returns the desired contribution of the sample
    sampleContributionOf :: a -> b -> Double
    sampleContributionOf _ _ = 1

    -- | computes the importance of the sample for metropolis-sampling
    sampleImportance :: a -> b -> Double
    sampleImportance a b = importanceFromCP (sampleContributionOf a b) (sampleProbabilityOf  a b)

    -- | construct a sampler which gets samples without importance
    sampleFrom :: a -> UCToMaybe b
    sampleFrom a = do
      swi <- sampleWithImportanceFrom a
      return $ sampledValue swi

  importanceFromCP contribution probability
    | contribution==0 = 0
    | otherwise       = contribution/probability

  -- | sample z of type a with its importance prepended. The importance is the product of
  -- | sample-contribution and the absolute jacobian determinant of its transformation from unitcubespace
  -- | i = c*ajd = c/p
  -- | which equals the reciprocal of the probability density |det(dS(u)/du)| = 1/p_S(u), z = S(u)

  {--continueSamplingFrom :: (Sampleable b c) => (a -> b) -> MaybeSampled a -> UCToMaybeSampled c
  continueSamplingFrom f ms = case ms of
      Nothing -> return Nothing
      Just s  -> do let v = f (sampledValue s)
                        oldi = sampledImportance s
                    ms' <- sampleWithImportanceFrom v
                    case ms' of
                      Nothing -> return Nothing
                      Just s' -> return . Just $ s' `scaleImportanceWith` oldi
  --}
    

  andWith f s1 s2 = (v1 `f` v2) `withImportance` (i1 * i2) where
    v1 = sampledValue s1
    v2 = sampledValue s2
    i1 = sampledImportance s1
    i2 = sampledImportance s2
