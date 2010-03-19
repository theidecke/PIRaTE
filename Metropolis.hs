{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BangPatterns #-}

module Metropolis where

  import Data.Word (Word64)
  import Data.List (foldl')
  import Data.Maybe (isNothing,isJust,fromJust)
  import System.Random.Mersenne.Pure64
  import Control.Monad.State
  import UCStream
  import Sampled
  
  
  class MetropolisDistribution a b | a -> b where
    -- | construct a sample (with importance) from an infinite number of streams
    -- | of infinite supply of random numbers ∈ [0,1]
    constructSampleWithImportance :: a -> [UCStream] -> Maybe (Sampled b)

  data MetropolisState = MetropolisState {
    msTree :: PerturbationTree,
    currentSampleWeight :: Double,
    decisions :: UCStream,
    freshTrees :: [PerturbationTree]
  }
  
  validSamplesAndTrees :: MetropolisDistribution a b => a -> [PerturbationTree] -> [(Sampled b, PerturbationTree)]
  validSamplesAndTrees mdist trees = filterJust (zip samples trees) where
    filterJust ((Nothing,_):sts) = filterJust sts
    filterJust (( Just s,t):sts) = (s,t) : filterJust sts
    samples = map (constructSampleFromTreeRoot mdist) trees

  metropolis :: MetropolisDistribution a b => a -> Word64 -> [Weighted b]
  metropolis mdist seed = initialsample : evalState (sequence . repeat $ step) initialstate where
    step = metropolisStep mdist
    initialsample = unitWeighing . sampledValue $ initialsample'
    initialstate = MetropolisState initialtree 0 decisions freshtrees
    freshtrees = mapSplit perturbationTreeFromGen g3
    (initialsample',initialtree) = head . validSamplesAndTrees mdist $ trees
    trees = mapSplit perturbationTreeFromGen g1 --possible starttrees
    decisions = randomUCStream g2
    (g1:g2:g3:_) = mapSplit id g
    g = pureMT seed

  metropolisStep :: MetropolisDistribution a b => a -> State MetropolisState (Weighted b)
  metropolisStep mdist = do
    d <- getDecision
    let step = if d < freshStepProbability then freshStep else perturbationStep
    step mdist

  freshStepProbability = 0.5
  
  perturbationStep :: MetropolisDistribution a b => a -> State MetropolisState (Weighted b)
  perturbationStep = metropolisStepFromTreeProposal getPerturbedTree

  freshStep        :: MetropolisDistribution a b => a -> State MetropolisState (Weighted b)
  freshStep        = metropolisStepFromTreeProposal getFreshTree

  metropolisStepFromTreeProposal :: MetropolisDistribution a b => State MetropolisState PerturbationTree -> a -> State MetropolisState (Weighted b)
  metropolisStepFromTreeProposal proposeTree mdist = do
    treeproposal <- proposeTree
    let mnewsample = constructSampleFromTreeRoot mdist treeproposal
    if isNothing mnewsample
      then metropolisStep mdist
      else do
        tree <- getCurrentTree
        csw <- getCurrentSampleWeight
        let Just currentsample = constructSampleFromTreeRoot mdist tree
            newsample = fromJust mnewsample
            a = computeAcceptanceProbability currentsample newsample
            nsw = csw + (1-a)
        d <- getDecision
        let acceptnewsample = d<=a
        if acceptnewsample
          then do
            substituteCurrentTreeWith treeproposal
            substituteCurrentSampleWeightWith a
            let csv = sampledValue currentsample
            return $ nsw `poundsOf` csv
          else do
            substituteCurrentSampleWeightWith nsw
            let nsv = sampledValue newsample
            return $   a `poundsOf` nsv

  getCurrentTree :: State MetropolisState PerturbationTree
  getCurrentTree = do
    state <- get
    return $ msTree state
  
  getFreshTree :: State MetropolisState PerturbationTree
  getFreshTree = do
    state <- get
    let (ft:fts) = freshTrees state
        state' = state {freshTrees = fts}
    put state'
    return ft

  getPerturbedTree :: State MetropolisState PerturbationTree
  getPerturbedTree = do
    tree <- getCurrentTree
    let headvariation = forwardToPertHead tree
        tree' = dropPertHead tree
    substituteCurrentTreeWith tree'
    return headvariation

  getDecision :: State MetropolisState Double
  getDecision = do
    state <- get
    let (d:ds) = decisions state
        state' = state {decisions = ds}
    put state'
    return d
  
  getCurrentSampleWeight :: State MetropolisState Double
  getCurrentSampleWeight = do
    state <- get
    return $ currentSampleWeight state

  substituteCurrentSampleWeightWith :: Double -> State MetropolisState ()
  substituteCurrentSampleWeightWith nsw = do
    state <- get
    let state' = state {currentSampleWeight = nsw}
    put state'
  
  substituteCurrentTreeWith :: PerturbationTree -> State MetropolisState ()
  substituteCurrentTreeWith tree = do
    state <- get
    let state' = state {msTree = tree}
    put state'

  computeAcceptanceProbability :: (Sampled b) -> (Sampled b) -> Double
  computeAcceptanceProbability s1 s2 = min 1 $ (sampledImportance s2) / (sampledImportance s1)
  
  constructSampleFromTreeRoot mdist = constructSampleWithImportance mdist . currentUCStreams

  type Weighted a = (Double,a)
  unitWeighing x = (1,x)
  poundsOf w x = (w,x)
  weightedWeight = fst
  weightedValue = snd
  
  expectationValue :: (a->Double) -> [Weighted a] -> Double
  expectationValue _ [] = error "can't compute expectation value without samples"
  expectationValue f samples = wv/tw where
    (tw,wv) = foldl' step (0,0) samples
    step (!accweight,!accvalue) (weight,sample) = (accweight',accvalue') where
      accweight' = accweight + weight
      accvalue'  = accvalue  + weight*(f sample)
