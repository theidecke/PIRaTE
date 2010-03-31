{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.Path.PathSamplerAtoms where
  import Data.ACVector
  import Data.Maybe (fromJust,fromMaybe)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (infinity)
  import PIRaTE.Scene.Scene
  import PIRaTE.MonteCarlo.Sampled

  import Debug.Trace


  data PointSampler = forall s . (Sampleable s Point) => PointSampler s
  instance Sampleable PointSampler Point where
    sampleProbabilityOf  (PointSampler s) = sampleProbabilityOf s
    sampleFrom           (PointSampler s) = sampleFrom s
    sampleContributionOf (PointSampler s) = sampleContributionOf s

  newtype SensationPointSampler = SensationPointSampler Scene
  instance Show SensationPointSampler where
    show (SensationPointSampler scene) = "SensationPointSampler in Scene: " ++ show scene
  instance Sampleable SensationPointSampler Point where
    sampleProbabilityOf (SensationPointSampler scene) origin =
      pointsamplerSamplingProbabilityOf scene sceneSensors origin
    sampleFrom (SensationPointSampler scene) =
      pointsamplerSampleFrom scene sceneSensors
    sampleContributionOf (SensationPointSampler scene) origin = scene `sensitivityAt` origin

  newtype EmissionPointSampler = EmissionPointSampler Scene
  instance Show EmissionPointSampler where
    show (EmissionPointSampler scene) = "EmissionPointSampler in Scene: " ++ show scene
  instance Sampleable EmissionPointSampler Point where
    sampleProbabilityOf (EmissionPointSampler scene) origin =
      pointsamplerSamplingProbabilityOf scene sceneEmitters origin
    sampleFrom (EmissionPointSampler scene) =
      pointsamplerSampleFrom scene sceneEmitters
    sampleContributionOf (EmissionPointSampler scene) origin = scene `emissivityAt` origin

  newtype ScatteringPointSampler = ScatteringPointSampler Scene
  instance Show ScatteringPointSampler where
    show (ScatteringPointSampler scene) = "ScatteringPointSampler in Scene: " ++ show scene
  instance Sampleable ScatteringPointSampler Point where
    sampleProbabilityOf (ScatteringPointSampler scene) origin =
      pointsamplerSamplingProbabilityOf scene sceneScatterers origin
    sampleFrom (ScatteringPointSampler scene) =
      pointsamplerSampleFrom scene sceneScatterers
    sampleContributionOf (ScatteringPointSampler scene) origin = scene `scatteringAt` origin

  pointsamplerSamplingProbabilityOf scene entityExtractor point =
      pointsamplingProbability entities point
    where entities = entityExtractor scene

  pointsamplingProbability entities point =
      sum [(sampleProbabilityOf container point) *
           (sampleProbabilityOf containers container) | container <- containers]
    where containers = map entityContainer entities

  pointsamplerSampleFrom scene entityExtractor
    | null entities = fail "can't sample PointSampler without Entities"
    | otherwise = do entity <- sampleFrom entities
                     let container = entityContainer entity
                     sampleFrom container
    where entities = entityExtractor scene

  data DirectionSampler = forall s . (Sampleable s Direction) => DirectionSampler s
  instance Sampleable DirectionSampler Direction where
    sampleProbabilityOf (DirectionSampler ds) = sampleProbabilityOf ds
    sampleFrom          (DirectionSampler ds) = sampleFrom ds
    sampleImportanceOf  (DirectionSampler ds) = sampleImportanceOf ds

  newtype SensationDirectionSampler = SensationDirectionSampler (Scene, Ray)
  instance Show SensationDirectionSampler where
    show (SensationDirectionSampler (scene,Ray origin _)) =
      "SensationDirectionSampler @" ++ showVector3 origin ++ "in Scene: " ++ show scene
  instance Sampleable SensationDirectionSampler Direction where
    sampleProbabilityOf (SensationDirectionSampler (scene,Ray origin _)) direction =
      sampleProbabilityOf (scene `sensorDirectednessesAt` origin, origin) direction
    sampleFrom (SensationDirectionSampler (scene,Ray origin _)) =
      sampleFrom (scene `sensorDirectednessesAt` origin,origin)
    -- contribution == probability
    sampleImportanceOf (SensationDirectionSampler _) _ = 1
    {-# INLINE sampleImportanceOf #-}


  newtype EmissionDirectionSampler = EmissionDirectionSampler (Scene, Ray)
  instance Show EmissionDirectionSampler where
    show (EmissionDirectionSampler (scene, Ray origin _)) =
      "EmissionDirectionSampler @" ++ showVector3 origin ++ "in Scene: " ++ show scene
  instance Sampleable EmissionDirectionSampler Direction where
    sampleProbabilityOf (EmissionDirectionSampler (scene, Ray origin _)) direction =
      sampleProbabilityOf (scene `emissionDirectednessesAt` origin, Ray origin (error "error: undefined10")) direction
    sampleFrom (EmissionDirectionSampler (scene, Ray origin _)) =
      sampleFrom (scene `emissionDirectednessesAt` origin, Ray origin (error "error: undefined11"))
    sampleImportanceOf (EmissionDirectionSampler _) _ = 1
    {-# INLINE sampleImportanceOf #-}


  newtype ScatteringDirectionSampler = ScatteringDirectionSampler (Scene, Ray)
  instance Show ScatteringDirectionSampler where
    show (ScatteringDirectionSampler (scene, Ray origin dir)) =
      "ScatteringDirectionSampler @" ++ showVector3 origin ++
      show dir ++
      "in Scene: " ++ show scene
  instance Sampleable ScatteringDirectionSampler Direction where
    sampleProbabilityOf (ScatteringDirectionSampler (scene, inray)) wout =
      sampleProbabilityOf (scene `scatterPhaseFunctionsAt` (rayOrigin inray), inray) wout
    sampleFrom (ScatteringDirectionSampler (scene, inray)) =
      sampleFrom (scene `scatterPhaseFunctionsAt` (rayOrigin inray), inray)
    sampleImportanceOf (ScatteringDirectionSampler _) _ = 1
    {-# INLINE sampleImportanceOf #-}

  -- Distance Samplers
  data DistanceSampler = forall s . (Sampleable s Distance) => DistanceSampler s

  instance Sampleable DistanceSampler Distance where
    sampleWithImportanceFrom (DistanceSampler ds) = sampleWithImportanceFrom ds
    {-# INLINE sampleWithImportanceFrom #-}
    sampleProbabilityOf  (DistanceSampler ds) = sampleProbabilityOf ds
    {-# INLINE sampleProbabilityOf #-}


  newtype SensorDistanceSampler = SensorDistanceSampler (Scene,Ray)
  instance Show SensorDistanceSampler where
    show (SensorDistanceSampler (scene, Ray origin dir)) =
      "SensorDistanceSampler @" ++ showVector3 origin ++ show dir ++ " in Scene: " ++ show scene
  instance Sampleable SensorDistanceSampler Distance where
    sampleWithImportanceFrom (SensorDistanceSampler (scene,outray))
      | totaldepth==0 = fail "no material ahead to sample a distance from."
      | otherwise = do u1 <- lift getCoord
                       let randomdepth = totaldepth * u1
                           proberesult = probeMedia (infinity, randomdepth)
                           mindist = 1e-14 -- avoids 0div-errors because of identical points
                           distance = (max mindist) . fromMaybe (error "SensorDistanceSampler.sampleWithImportanceFrom: distance") . getProbeResultDist $ proberesult
                           opticaldepth = fromMaybe (error "SensorDistanceSampler.sampleWithImportanceFrom: opticaldepth") . getProbeResultDepth $ probeExtinction scene outray distance infinity
                           attenuation = exp (-opticaldepth) -- /distance^2 --cancels with geometric term in RecursivePointSampler probability
                           importance = attenuation * totaldepth
                       return $ distance `withImportance` importance
      where totaldepth = fromMaybe (error "SensorDistanceSampler.sampleWithImportanceFrom: totaldepth") $ getProbeResultDepth totaldepthproberesult
            totaldepthproberesult = probeMedia (infinity, infinity)
            probeMedia = probeSensitivityClosure scene outray

    sampleProbabilityOf _ _ = (error "error: undefined12")


  newtype EmitterDistanceSampler = EmitterDistanceSampler (Scene,Ray)
  instance Show EmitterDistanceSampler where
    show (EmitterDistanceSampler (scene, Ray origin dir)) =
      "EmitterDistanceSampler @" ++ showVector3 origin ++ show dir ++ " in Scene: " ++ show scene
  instance Sampleable EmitterDistanceSampler Distance where
    sampleWithImportanceFrom (EmitterDistanceSampler (scene,outray))
      | totaldepth==0 = fail "no material ahead to sample a distance from."
      | otherwise = do u1 <- lift getCoord
                       let randomdepth = totaldepth * u1
                           proberesult = probeMedia (infinity, randomdepth)
                           mindist = 1e-14 -- avoids 0div-errors because of identical points
                           distance = (max mindist) . fromMaybe (error "EmitterDistanceSampler.sampleWithImportanceFrom: distance") . getProbeResultDist $ proberesult
                           opticaldepth = fromMaybe (error "EmitterDistanceSampler.sampleWithImportanceFrom: opticaldepth") . getProbeResultDepth $ probeExtinction scene outray distance infinity
                           attenuation = exp (-opticaldepth) -- /distance^2 --cancels with geometric term in RecursivePointSampler probability
                           importance = attenuation * totaldepth
                       return $ distance `withImportance` importance
      where totaldepth = fromMaybe (error "EmitterDistanceSampler.sampleWithImportanceFrom: totaldepth") $ getProbeResultDepth totaldepthproberesult
            totaldepthproberesult = probeMedia (infinity, infinity)
            probeMedia = probeEmissivityClosure scene outray

    sampleProbabilityOf _ _ = (error "error: undefined13")


  newtype ScattererDistanceSampler = ScattererDistanceSampler (Scene,Ray)
  instance Show ScattererDistanceSampler where
    show (ScattererDistanceSampler (scene, Ray origin dir)) =
      "ScattererDistanceSampler @" ++ showVector3 origin ++ show dir ++ " in Scene: " ++ show scene
  instance Sampleable ScattererDistanceSampler Distance where
    -- UniformAttenuationDistanceSampler
    {--
    sampleWithImportanceFrom (ScattererDistanceSampler (scene,outray)) = do
      u1 <- lift getCoord
      let depth = negate $ log (u1::Double)
          proberesult = probeExtinctionClosure scene outray (infinity,depth)
          mdistance = getProbeResultDist proberesult
      case mdistance of
        Nothing       -> fail "depth overshoot"
        Just distance -> do
          let endpoint = outray `followFor` distance
              albedo = scene `albedoAt` endpoint
          return $ distance `withImportance` albedo--}

    --UniformAvailableAttenuationDistanceSampler
    sampleWithImportanceFrom (ScattererDistanceSampler (scene,outray))
      | totaldepth==0 = fail "no material ahead to sample a distance from."
      | otherwise = do
          u1 <- lift getCoord
          let absorptionatinfinity = (1 - (exp (-totaldepth)))
              endpointdepth = if totaldepth > 1e-5
                then negate $ log (1 - absorptionatinfinity * u1)
                else totaldepth*u1 * (1 + 0.5*totaldepth*(u1-1))
              proberesult = probeMedia (infinity, endpointdepth)
              distance = fromMaybe (error (distanceerrormsg totaldepth endpointdepth u1)) . getProbeResultDist $ proberesult
              endpoint = outray `followFor` distance
              albedo = scene `albedoAt` endpoint
              importance = albedo * absorptionatinfinity
              mindist = 1e-14 -- avoids 0div-errors because of identical points
          return $ max mindist distance `withImportance` importance
      where totaldepth = fromMaybe (error "UniformAvailableAttenuationDistanceSampler.sampleWithImportanceFrom: totaldepth") . getProbeResultDepth $ probeMedia (infinity, infinity)
            probeMedia = probeExtinctionClosure scene outray
            distanceerrormsg td epd u = "UniformAvailableAttenuationDistanceSampler.sampleWithImportanceFrom: distance totaldepth=" ++ show td ++ " endpointdepth=" ++ show epd ++ " u1="++show u


    sampleProbabilityOf _ _ = (error "error: undefined14")
