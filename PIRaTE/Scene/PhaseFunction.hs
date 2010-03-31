{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.Scene.PhaseFunction where
  import qualified Data.WeighedSet as WS
  import PIRaTE.SpatialTypes
  import PIRaTE.MonteCarlo.Sampled
  import PIRaTE.MonteCarlo.UtilitySamplers (randomWeightedChoice)
  
  -- PhaseFunction determines the Direction-dependent scattering probability
  data PhaseFunction = forall pf. (Sampleable (pf,Ray) Direction, Show pf) => PhaseFunction pf

  instance Show PhaseFunction where
    show (PhaseFunction pf) = show pf

  instance Sampleable (PhaseFunction,Ray) Direction where
    sampleProbabilityOf (PhaseFunction pf,inray) wout = sampleProbabilityOf (pf,inray) wout
    {-# INLINE sampleProbabilityOf #-}
    sampleWithImportanceFrom (PhaseFunction pf,inray) = sampleWithImportanceFrom (pf,inray)
    {-# INLINE sampleWithImportanceFrom #-}

  type WeightedPhaseFunction = [(PhaseFunction,Double)]
  
  instance Sampleable (WeightedPhaseFunction,Ray) Direction where
    sampleProbabilityOf (wpflist,inray) wout
      | totalweight==0 = 0
      | otherwise = (/totalweight) . sum $ [w*(sampleProbabilityOf (pf,inray) wout) | (pf,w) <- wpflist]
      where totalweight = sum . snd . unzip $ wpflist

    sampleFrom (wpf,inray)
      | null wpf = fail "no wpfs to choose from"
      | otherwise = do
          sampledphasefunction <- lift . randomWeightedChoice $ wpf
          let pf = sampledValue $ sampledphasefunction
          sampleFrom (pf,inray)
