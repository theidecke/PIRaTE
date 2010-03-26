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
    sampleWithImportanceFrom     (PhaseFunction pf,inray) = sampleWithImportanceFrom (pf,inray)
    {-# INLINE sampleWithImportanceFrom #-}

  newtype IndexedPhaseFunction = IndexedPhaseFunction {ipfPairForm :: (Int,PhaseFunction)}
  
  instance Show IndexedPhaseFunction where
    show (IndexedPhaseFunction (index,_)) = "Phasefunction " ++ show index
  instance Eq IndexedPhaseFunction where
    (==) (IndexedPhaseFunction (index1,_)) (IndexedPhaseFunction (index2,_)) = index1==index2
    {-# INLINE (==) #-}
  instance Ord IndexedPhaseFunction where
    (<=) (IndexedPhaseFunction (index1,_)) (IndexedPhaseFunction (index2,_)) = index1 <= index2
    {-# INLINE (<=) #-}
    
    
  type WeightedPhaseFunction = [(PhaseFunction,Double)]
  
  instance Sampleable (WeightedPhaseFunction,Ray) Direction where
    sampleProbabilityOf (wpflist,inray) wout
      | totalweight==0 = 0
      | otherwise = (/totalweight) . sum $ [w*(sampleProbabilityOf (pf,inray) wout) | (pf,w) <- wpflist]
      where totalweight = sum . snd . unzip $ wpflist

    sampleWithImportanceFrom (wpf,inray) = do
      sampledphasefunction <- lift . randomWeightedChoice $ wpf
      let pf = sampledValue $ sampledphasefunction
      sampledwout <- sampleWithImportanceFrom (pf,inray)
      let wout = sampledValue sampledwout
      return $ wout `withProbability` sampleProbabilityOf (wpf,inray) wout
    {-# INLINE sampleWithImportanceFrom #-}