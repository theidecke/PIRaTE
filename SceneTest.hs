{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
  import Data.ACVector
  import qualified Data.WeighedSet as WS
  import Data.List (intersperse,foldl',sortBy)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (infinity,edgeMap)
  import PIRaTE.Scene.Confineable
  import PIRaTE.Scene.Container
  import PIRaTE.Scene.Container.Sphere
  import PIRaTE.Scene.Container.Box
  import PIRaTE.Scene.PhaseFunction
  import PIRaTE.Scene.PhaseFunction.Isotropic
  import PIRaTE.Scene.PhaseFunction.ZCone
  import PIRaTE.Scene.Texture
  import PIRaTE.Scene.Material
  import PIRaTE.Scene.Sensor
  import PIRaTE.Scene.Scene
  import PIRaTE.MonteCarlo.Sampled
  import PIRaTE.MonteCarlo.Metropolis
  import PIRaTE.MonteCarlo.UCStream (toStream)
  import PIRaTE.Path.PathSamplerAtoms
  import PIRaTE.Path.PathGenerators
  import Control.Monad.State
  import Control.Monad.Maybe
  import Control.Applicative
  import Text.Printf (printf)
  import System.Environment (getArgs)
  
  standardScene sigma = let
      emissionphasefunction   = PhaseFunction Isotropic
      scatteringphasefunction = PhaseFunction Isotropic
      sensationphasefunction  = (PhaseFunction $ fromApexAngle sensorangle, PathLength . mltStatePathLength)
      lightsourcecontainer = Container $ Sphere (Vector3 0 0 0) 0.005
      lightsourcematerial = toHomogenousEmittingMaterial 1.0 emissionphasefunction
      lightsourceentity = entityFromContainerAndMaterials lightsourcecontainer [lightsourcematerial]
      scatteringcontainer = Container $ Sphere (Vector3 0 0 0) 1
      scatteringmaterial = toHomogenousInteractingMaterial 0 sigma scatteringphasefunction
      scatteringentity = entityFromContainerAndMaterials scatteringcontainer [scatteringmaterial]
      sensorcontainer  = Container $ fromCorners (Vector3 (-1) (-1) (-4.01)) (Vector3 1 1 (-3.99))
      sensormaterial = toHomogenousSensingMaterial 1.0 sensationphasefunction
      sensorangle = 0.1 * degree
      sensorentity  = entityFromContainerAndMaterials sensorcontainer  [sensormaterial]
      entities = [lightsourceentity,scatteringentity,sensorentity]
    in sceneFromEntities entities

  testsampler = EmissionPointSampler $ standardScene 2.3

  newtype StupidMetropolisDistribution = StupidMetropolisDistribution (Scene,Double) deriving Show
  instance MetropolisDistribution StupidMetropolisDistribution MLTState where
    --constructSampleWithImportance :: a -> [UCStream] -> Maybe (Sampled b)
    constructSampleWithImportance (StupidMetropolisDistribution (scene, growprobability)) (stream:_) =
        runUCToMaybeSampled (sampleWithImportanceFrom pathgenerator) stream
      where pathgenerator = StupidPathGenerator (scene, growprobability)

  newtype SimpleMetropolisDistribution = SimpleMetropolisDistribution (Scene,Double) deriving Show
  instance MetropolisDistribution SimpleMetropolisDistribution MLTState where
    --constructSampleWithImportance :: a -> [UCStream] -> Maybe (Sampled b)
    constructSampleWithImportance (SimpleMetropolisDistribution (scene, growprobability)) (stream:_) =
        runUCToMaybeSampled (sampleWithImportanceFrom pathgenerator) stream
      where pathgenerator = SimplePathGenerator (scene, growprobability)

  newtype PathTracerMetropolisDistribution = PathTracerMetropolisDistribution (Scene,Double) deriving Show
  instance MetropolisDistribution PathTracerMetropolisDistribution MLTState where
    --constructSampleWithImportance :: a -> [UCStream] -> Maybe (Sampled b)
    constructSampleWithImportance (PathTracerMetropolisDistribution (scene, growprobability)) (stream:_) =
        runUCToMaybeSampled (sampleWithImportanceFrom pathgenerator) stream
      where pathgenerator = SimplePathtracerPathGenerator (scene, growprobability)

  stdscene = standardScene 2.0
  growprob = 0.5
  smd1 = StupidMetropolisDistribution (stdscene, growprob)
  smd2 = SimpleMetropolisDistribution (stdscene, growprob)
  smd3 = PathTracerMetropolisDistribution (stdscene, growprob)

  getSamples smd seed = map (\(w,p)->(w,(\v->(v3x v,v3y v)) . last $ p)) . metropolis smd $ seed
  showSample :: (Double,(Double,Double)) -> String
  showSample (w,(x,y)) = printf "{%f,{%f,%f}}" w x y
  showDouble :: Double -> String
  showDouble = printf "%f"
  showListForMathematica showElement xs = "{"++ concat (intersperse "," (map showElement xs)) ++ "}"
  --showListForMathematica showSample . take 10 $ samples
  showGrid2DForMathematica = showListForMathematica (showListForMathematica showDouble)

  getBinnedSamples seed gridsize n =
    showGrid2DForMathematica . binSamplesInGrid gridsize . take n . getSamples smd3 $ seed
  
  main = do
    args <- getArgs
    let n = read . head $ args
        samples = take n . getSamples smd3 $ 13
    putRadiallyBinnedPhotonCounts 100 samples
    --putGridBinnedPhotonCounts 101 samples
  
  binSamplesInGrid :: Int -> [(Double, (Double,Double))] -> [[Double]]
  binSamplesInGrid n samples = let
      sampleMap f (w,s) = (w,f s)
      pairMap f (x,y) = (f x, f y)

      gridIndices :: Int -> [[(Int,Int)]]
      gridIndices n = [[(i,j) | i<-indices1d] | j <- reverse indices1d]
        where indices1d = [0..n-1]

      toUnitSquare :: [(Double, (Double,Double))] -> [(Double, (Double,Double))]
      toUnitSquare samples = map (sampleMap (pairMap (\x -> 0.5*(x+1)))) samples

      coordsToGridIndex :: Int -> (Double,Double) -> (Int,Int)
      coordsToGridIndex n point = pairMap (\x -> truncate $ (fromIntegral n)*x) point

      emptybins = WS.empty
      sampleindices = map (sampleMap (coordsToGridIndex n)) (toUnitSquare samples)
      considerSample ws (w,s) = WS.increaseWeightBy w ws s
      fullbins = foldl' considerSample emptybins sampleindices
    in map (map $ WS.weightOf fullbins) (gridIndices n)
  
  binSamplesRadially :: Int -> [(Double, (Double,Double))] -> [Double]
  binSamplesRadially n samples = let
      sampleMap f (w,s) = (w,f s)

      gridIndices :: Int -> [Int]
      gridIndices n = [0..n-1]

      toCenterDistance :: [(Double, (Double,Double))] -> [(Double,Double)]
      toCenterDistance samples = map (sampleMap (\(x,y) -> sqrt (x*x+y*y))) samples

      centerDistanceToBinIndex :: Int -> Double -> Int
      centerDistanceToBinIndex n distance = truncate $ (fromIntegral n)*distance

      emptybins = WS.empty
      sampleindices = map (sampleMap (centerDistanceToBinIndex n)) (toCenterDistance samples)
      considerSample ws (w,s) = WS.increaseWeightBy w ws s
      fullbins = foldl' considerSample emptybins sampleindices
    in map (WS.weightOf fullbins) (gridIndices n)
  
  putGridBinnedPhotonCounts gridsize samples = do
    let photonbincounts = binSamplesInGrid gridsize samples
    putStrLn $ "binnedphotons=" ++ (showGrid2DForMathematica photonbincounts) ++ ";\n"
    
  putRadiallyBinnedPhotonCounts gridsize samples = do
    let photonbincounts = binSamplesRadially gridsize samples
    putStrLn $ "radialphotoncounts=" ++ (showListForMathematica showDouble photonbincounts) ++ ";\n"

    