{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Playground where

  import Data.Word (Word64)
  import Data.List (unfoldr)
  import Data.Maybe (isJust,fromJust)
  import System.Random.Mersenne.Pure64
  import UCStream
  import Sampled
  
  
  class MetropolisDistribution a b | a -> b where
    -- | construct a sample (with importance) from an infinite number of streams
    -- | of infinite supply of random numbers ∈ [0,1]
    constructSampleWithImportance :: a -> [UCStream] -> Maybe (Sampled b)

  type Weighted a = (Double,a)
  unitWeighing x = (1,x)

  data MetropolisState = MetropolisState {
    msTree :: PerturbationTree,
    currentSampleWeight :: Double
  }
  
  validSamplesAndTrees :: MetropolisDistribution a b => a -> [PerturbationTree] -> [(Sampled b, PerturbationTree)]
  validSamplesAndTrees mdist trees = filterJust (zip samples trees) where
    filterJust ((Nothing,_):sts) = filterJust sts
    filterJust (( Just s,t):sts) = (s,t) : filterJust sts
    samples = map (constructSampleWithImportance mdist . currentUCStreams) trees
  
  metropolis :: MetropolisDistribution a b => a -> Word64 -> [Weighted b]
  metropolis mdist seed = initialsample : unfoldr step initialstate where
    step = metropolisStep mdist
    initialsample = unitWeighing . sampledValue $ initialsample'
    initialstate = MetropolisState initialtree 0
    (initialsample',initialtree) = head . validSamplesAndTrees mdist $ trees
    trees = mapSplit perturbationTreeFromGen g --possible starttrees
    g = pureMT seed
  
  metropolisStep :: MetropolisDistribution a b => a -> MetropolisState -> Maybe (Weighted b, MetropolisState)
  metropolisStep mdist (MetropolisState tree csw) = case mnewsample of
      Nothing -> metropolisStep mdist (MetropolisState nextvariation csw) --retry until we construct a valid sample
      _ -> Nothing
    where
      mnewsample = constructSampleWithImportance mdist currentstreams
      currentstreams = currentUCStreams tree
      nextvariation = dropPertHead tree

  {--
  unfold      :: (b -> Maybe (a, b)) -> b -> [a]
  unfold f b  =
    case f b of
     Just (a,new_b) -> a : unfoldr f new_b
     Nothing        -> []
     
  mapMaybe :: (a -> Maybe b) -> [a] -> [b]
  --}