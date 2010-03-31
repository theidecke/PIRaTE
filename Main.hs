{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
  import Data.ACVector
  import qualified Data.WeighedSet as WS
  import Data.List (intersperse,foldl',sortBy)
  import Control.Monad.State
  import Control.Monad.Maybe
  import Control.Applicative
  import Text.Printf (printf)
  import System.Environment (getArgs)
  import Control.Parallel
  import Control.Parallel.Strategies
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

  testScene =    let cont1 = Container $ Sphere (Vector3 0.3 0 0) 0.5
                     cont2 = Container $ Sphere (Vector3 (-0.5) 0 0) 0.3
                     cont3 = Container $ Sphere (Vector3 0.2 0.1 (-0.15)) 0.1
                     cont4 = Container $ Sphere (Vector3 (-0.35) (-0.7) 0.0) 0.25
                     emissionphasefunction   = PhaseFunction Isotropic
                     scatteringphasefunction = PhaseFunction Isotropic
                     sensationphasefunction  = (PhaseFunction $ fromApexAngle sensorangle, PathLength . mltStatePathLength)
                     mat1 = toHomogenousInteractingMaterial  3  4 scatteringphasefunction
                     mat2 = toHomogenousInteractingMaterial  0  7 scatteringphasefunction
                     mat3 = toHomogenousInteractingMaterial 40  0 scatteringphasefunction
                     mat4 = toHomogenousInteractingMaterial  0 40 scatteringphasefunction
                     ent1 = entityFromContainerAndMaterials cont1 [mat1]
                     ent2 = entityFromContainerAndMaterials cont2 [mat2]
                     ent3 = entityFromContainerAndMaterials cont3 [mat3]
                     ent4 = entityFromContainerAndMaterials cont4 [mat4]
                     sensorcontainer = Container $ fromCorners (Vector3 (-1) (-1) (-1.02)) (Vector3 1 1 (-1.01))
                     sensormaterial = toHomogenousSensingMaterial 1.0 sensationphasefunction
                     sensorangle = 1 * degree
                     sensorentity = entityFromContainerAndMaterials sensorcontainer [sensormaterial]
                     lightsourcecontainer = Container $ Sphere (Vector3 0 0 0) 0.01
                     lightsourcematerial = toHomogenousEmittingMaterial 1.0 emissionphasefunction
                     lightsourceentity = entityFromContainerAndMaterials lightsourcecontainer [lightsourcematerial]
                 in sceneFromEntities [ent1,ent2,ent3,ent4,sensorentity,lightsourceentity]

  standardScene sigma = let
      emissionphasefunction   = PhaseFunction Isotropic
      scatteringphasefunction = PhaseFunction Isotropic
      sensationphasefunction  = (PhaseFunction $ fromApexAngle sensorangle, PathLength . mltStatePathLength)
      lightsourcecontainer = Container $ Sphere (Vector3 0 0 0) 0.001
      lightsourcematerial = toHomogenousEmittingMaterial 1.0 emissionphasefunction
      lightsourceentity = entityFromContainerAndMaterials lightsourcecontainer [lightsourcematerial]
      scatteringcontainer = Container $ Sphere (Vector3 0 0 0) 1
      scatteringmaterial = toHomogenousInteractingMaterial 0 sigma scatteringphasefunction
      scatteringentity = entityFromContainerAndMaterials scatteringcontainer [scatteringmaterial]
      sensorcontainer  = Container $ fromCorners (Vector3 (-1) (-1) (-1.02)) (Vector3 1 1 (-1.01))
      sensormaterial = toHomogenousSensingMaterial 1.0 sensationphasefunction
      sensorangle = 25 * arcmin
      sensorentity  = entityFromContainerAndMaterials sensorcontainer  [sensormaterial]
      entities = [lightsourceentity,scatteringentity,sensorentity]
    in sceneFromEntities entities

  inhomScene sigma = let
      emissionphasefunction   = PhaseFunction Isotropic
      scatteringphasefunction = PhaseFunction Isotropic
      sensationphasefunction  = (PhaseFunction $ fromApexAngle sensorangle, PathLength . mltStatePathLength)
      lightsourcecontainer = Container $ Sphere (Vector3 0 0 0) 0.001
      lightsourcematerial = toHomogenousEmittingMaterial 1.0 emissionphasefunction
      lightsourceentity = entityFromContainerAndMaterials lightsourcecontainer [lightsourcematerial]
      scatteringcontainer = Container $ Sphere (Vector3 0 0 0) 1
      scatteringmaterial = toCustomInteractingMaterial Empty (Inhomogenous sigmafun) scatteringphasefunction
      sigmafun = simpleDisc sigma 0.1 1.0 0.25
      simpleDisc m eps so a = rho where
        rho p = if s<0.01 || s>1 then 0 else c * (exp (-0.5*(z/(eps*s))^2)) / (a^2+s^2) where {z=v3y p; s=sqrt ((v3x p)^2+(v3z p)^2)}
        c = m / ((2*pi)**1.5 * eps * (so - a*(atan (so/a))))
      scatteringentity = entityFromContainerAndMaterials scatteringcontainer [scatteringmaterial]
      sensorcontainer = Container $ fromCorners (Vector3 (-1) (-0.4) (-1.02)) (Vector3 1 0.4 (-1.01))
      sensormaterial = toHomogenousSensingMaterial 1.0 sensationphasefunction
      sensorangle = 25 * arcmin
      sensorentity = entityFromContainerAndMaterials sensorcontainer [sensormaterial]
      entities = [lightsourceentity, scatteringentity,sensorentity]
    in sceneFromEntities entities

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

  newtype PathTracerMetropolisDistribution = PathTracerMetropolisDistribution Scene deriving Show
  instance MetropolisDistribution PathTracerMetropolisDistribution MLTState where
    --constructSampleWithImportance :: a -> [UCStream] -> Maybe (Sampled b)
    constructSampleWithImportance (PathTracerMetropolisDistribution scene) (stream:_) =
        runUCToMaybeSampled (sampleWithImportanceFrom pathgenerator) stream
      where pathgenerator = SimplePathtracerPathGenerator scene

  main = do
    args <- getArgs
    let (gridsize,n) = ((read (args!!0))::Int
                       ,(read (args!!1))::Int)
    let --scene = inhomScene 1.0
        scene = testScene
        metropolisdistribution = PathTracerMetropolisDistribution scene
        extractor = (\(w,p)->(w,(\v->(v3x v,v3y v)) . last $ p))
        startSampleSession size seed = take size . map extractor . metropolis metropolisdistribution $ fromIntegral seed
        sessionsize = min 10000 n --n
        sessioncount = n `div` sessionsize
        samplesessions = map (startSampleSession sessionsize) [1..sessioncount]
        samples = concat (samplesessions `using` parList rdeepseq)
    --putRadiallyBinnedPhotonCounts gridsize samples
    putGridBinnedPhotonCounts gridsize samples

  putGridBinnedPhotonCounts gridsize samples = do
    let photonbincounts = binSamplesInGrid gridsize samples
    putStrLn $ "binnedphotons=" ++ (showGrid2DForMathematica photonbincounts) ++ ";\n"
    
  putRadiallyBinnedPhotonCounts gridsize samples = do
    let photonbincounts = binSamplesRadially gridsize samples
    putStrLn $ "radialphotoncounts=" ++ (showListForMathematica showDouble photonbincounts) ++ ";\n"

  showSample :: (Double,(Double,Double)) -> String
  showSample (w,(x,y)) = printf "{%f,{%f,%f}}" w x y
  showDouble :: Double -> String
  showDouble = printf "%f"
  showListForMathematica showElement xs = "{"++ concat (intersperse "," (map showElement xs)) ++ "}"
  --showListForMathematica showSample . take 10 $ samples
  showGrid2DForMathematica = showListForMathematica (showListForMathematica showDouble)

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
