{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.Scene.Container where
  import Data.ACVector ((|*),vdot)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq)
  import PIRaTE.Scene.Confineable
  import PIRaTE.MonteCarlo.Sampled
  
  -- stuff lives in Containers,
  data Container = forall c. (Confineable c, Sampleable c Point, Show c) => Container c

  instance Show Container where
    show (Container c) = show c
  
  instance Confineable Container where
    contains      (Container c) = contains c
    {-# INLINE contains #-}
    intersectedBy (Container c) = intersectedBy c
    {-# INLINE intersectedBy #-}

  instance Sampleable Container Point where
    sampleProbabilityOf (Container c) p = sampleProbabilityOf c p
    {-# INLINE sampleProbabilityOf #-}
    sampleWithImportanceFrom (Container c) = sampleWithImportanceFrom c
    {-# INLINE sampleWithImportanceFrom #-}

    