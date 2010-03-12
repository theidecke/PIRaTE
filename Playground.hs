{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Playground where

  import Data.Word (Word64)
  import Data.List (unfoldr)
  import Data.Maybe (isNothing,isJust,fromJust)
  import System.Random.Mersenne.Pure64
  import UCStream
  import Sampled
  
  
  class MetropolisDistribution a b | a -> b where
    -- | construct a sample (with importance) from an infinite number of streams
    -- | of infinite supply of random numbers ∈ [0,1]
    constructSampleWithImportance :: a -> [UCStream] -> Maybe (Sampled b)

  type Weighted a = (Double,a)
  unitWeighing x = (1,x)
  poundsOf w x = (w,x)

  data MetropolisState = MetropolisState {
    msTree :: PerturbationTree,
    currentSampleWeight :: Double,
    acceptanceDecisions :: UCStream
  }
  
  validSamplesAndTrees :: MetropolisDistribution a b => a -> [PerturbationTree] -> [(Sampled b, PerturbationTree)]
  validSamplesAndTrees mdist trees = filterJust (zip samples trees) where
    filterJust ((Nothing,_):sts) = filterJust sts
    filterJust (( Just s,t):sts) = (s,t) : filterJust sts
    samples = map (constructSampleFromTreeRoot mdist) trees
  
  metropolis :: MetropolisDistribution a b => a -> Word64 -> [Weighted b]
  metropolis mdist seed = initialsample : unfoldr step initialstate where
    step = Just . metropolisStep mdist
    initialsample = unitWeighing . sampledValue $ initialsample'
    initialstate = MetropolisState initialtree 0 acceptancedecisions
    (initialsample',initialtree) = head . validSamplesAndTrees mdist $ trees
    trees = mapSplit perturbationTreeFromGen g1 --possible starttrees
    acceptancedecisions = randomUCStream g2
    (g1:g2:_) = mapSplit id g
    g = pureMT seed

  type MetropolisStepResult b = (Weighted b, MetropolisState)

  metropolisStep :: MetropolisDistribution a b => a -> MetropolisState -> MetropolisStepResult b
  metropolisStep mdist ms@(MetropolisState tree csw ads)
    | isNothing mnewsample = metropolisStep mdist (MetropolisState nextvariation csw ads) --retry until we construct a valid sample
    | acceptnewsample = (nsw `poundsOf` csv, MetropolisState headvariation a   ds)
    | otherwise       = (  a `poundsOf` nsv, MetropolisState nextvariation nsw ds)
    where
      acceptnewsample = decision <= a
      nsw = csw + (1-a)
      a = computeAcceptanceProbability currentsample newsample
      nsv = sampledValue newsample
      csv = sampledValue currentsample
      newsample = fromJust mnewsample
      mnewsample         = constructSampleFromTreeRoot mdist headvariation
      Just currentsample = constructSampleFromTreeRoot mdist tree
      headvariation = forwardToPertHead tree
      nextvariation = dropPertHead tree
      (decision:ds) = ads
  
  computeAcceptanceProbability :: (Sampled b) -> (Sampled b) -> Double
  computeAcceptanceProbability s1 s2 = min 1 $ (sampledImportance s2) / (sampledImportance s1)
  
  constructSampleFromTreeRoot mdist = constructSampleWithImportance mdist . currentUCStreams

  data Gauss2DCartesian = Gauss2DCartesian
  instance MetropolisDistribution Gauss2DCartesian (Double,Double) where
    constructSampleWithImportance _ ((u1:u2:_):_) = Just (toSampled value importance) where
      value = (x,y)
      (x,y) = (2*u1-1,2*u2-1)
      importance = exp . negate . (30*) $ x^2 + y^2

  data Gauss2DPolar = Gauss2DPolar
  instance MetropolisDistribution Gauss2DPolar (Double,Double) where
    constructSampleWithImportance _ ((u1:u2:_):_) = Just (toSampled value importance) where
      importance = contribution / probability
      contribution = exp . negate . (30*) $ r^2
      value = (r*(cos phi),r*(sin phi))
      probability = 1/r
      r   = u1
      phi = 2*pi*u2

  -- concatMap ((\(w,(x,y))->printf "{%f,{%f,%f}}," w x y)) . take 20000 . metropolis TestProblem2 $ 46 :: String