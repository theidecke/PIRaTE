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


  newtype EmissionDirectionSampler = EmissionDirectionSampler (Scene, Ray)
  instance Show EmissionDirectionSampler where
    show (EmissionDirectionSampler (scene, Ray origin _)) =
      "EmissionDirectionSampler @" ++ showVector3 origin ++ "in Scene: " ++ show scene
  instance Sampleable EmissionDirectionSampler Direction where
    sampleProbabilityOf (EmissionDirectionSampler (scene, Ray origin _)) direction =
      sampleProbabilityOf (scene `emissionDirectednessesAt` origin, Ray origin undefined) direction
    sampleFrom (EmissionDirectionSampler (scene, Ray origin _)) =
      sampleFrom (scene `emissionDirectednessesAt` origin, Ray origin undefined)
    sampleImportanceOf (EmissionDirectionSampler _) _ = 1


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

  -- Distance Samplers
  data DistanceSampler = forall s . (Sampleable s Distance) => DistanceSampler s

  instance Sampleable DistanceSampler Distance where
    sampleWithImportanceFrom (DistanceSampler ds) = sampleWithImportanceFrom ds
    sampleProbabilityOf  (DistanceSampler ds) = sampleProbabilityOf ds
    sampleFrom           (DistanceSampler ds) = sampleFrom ds
    sampleContributionOf (DistanceSampler ds) = sampleContributionOf ds
    sampleImportanceOf   (DistanceSampler ds) = sampleImportanceOf ds


  newtype SensorDistanceSampler = SensorDistanceSampler (Scene,Ray)
  instance Show SensorDistanceSampler where
    show (SensorDistanceSampler (scene, Ray origin dir)) =
      "SensorDistanceSampler @" ++ showVector3 origin ++ show dir ++ " in Scene: " ++ show scene
  instance Sampleable SensorDistanceSampler Distance where
    sampleWithImportanceFrom (SensorDistanceSampler (scene, outray)) = do
        sampleddistance <- sampleWithImportanceFrom distsampler
        let distance = sampledValue sampleddistance
            endpoint = outray `followFor` distance
            endpointcontribution = scene `sensitivityAt` endpoint
            distanceimportance = sampledImportance sampleddistance
            importance = distanceimportance * endpointcontribution
        return $ distance `withImportance` importance
      where distsampler = getSensorDistanceSampler scene outray

    sampleProbabilityOf (SensorDistanceSampler (scene, outray)) distance =
        sampleProbabilityOf distsampler distance
      where distsampler = getSensorDistanceSampler scene outray

    sampleFrom (SensorDistanceSampler (scene, outray)) =
        sampleFrom distsampler
      where distsampler = getSensorDistanceSampler scene outray

    sampleContributionOf (SensorDistanceSampler (scene, outray)) distance = contribution
      where contribution = endpointcontribution * edgecontribution
            endpointcontribution = scene `sensitivityAt` endpoint
            endpoint = outray `followFor` distance
            edgecontribution = sampleContributionOf distsampler distance
            distsampler = getSensorDistanceSampler scene outray

    sampleImportanceOf (SensorDistanceSampler (scene, outray)) distance =
        sampleImportanceOf distsampler distance
      where distsampler = getSensorDistanceSampler scene outray

  getSensorDistanceSampler scene outray = UniformDepthDistanceSampler (scene, outray, depthclosure, pointproperty) where
    depthclosure  = probeSensitivityClosure
    pointproperty = sensitivityAt

  newtype EmitterDistanceSampler = EmitterDistanceSampler (Scene,Ray)
  instance Show EmitterDistanceSampler where
    show (EmitterDistanceSampler (scene, Ray origin dir)) =
      "EmitterDistanceSampler @" ++ showVector3 origin ++ show dir ++ " in Scene: " ++ show scene
  instance Sampleable EmitterDistanceSampler Distance where
    sampleWithImportanceFrom (EmitterDistanceSampler (scene, outray)) = do
        sampleddistance <- sampleWithImportanceFrom distsampler
        let distance = sampledValue sampleddistance
            endpoint = outray `followFor` distance
            endpointcontribution = scene `emissivityAt` endpoint
            distanceimportance = sampledImportance sampleddistance
            importance = distanceimportance * endpointcontribution
        return $ distance `withImportance` importance
      where distsampler = getEmitterDistanceSampler scene outray

    sampleProbabilityOf (EmitterDistanceSampler (scene, outray)) distance =
        sampleProbabilityOf distsampler distance
      where distsampler = getEmitterDistanceSampler scene outray

    sampleFrom (EmitterDistanceSampler (scene, outray)) =
        sampleFrom distsampler
      where distsampler = getEmitterDistanceSampler scene outray

    sampleContributionOf (EmitterDistanceSampler (scene, outray)) distance = contribution
      where contribution = endpointcontribution * edgecontribution
            endpointcontribution = scene `emissivityAt` endpoint
            endpoint = outray `followFor` distance
            edgecontribution = sampleContributionOf distsampler distance
            distsampler = getEmitterDistanceSampler scene outray

    sampleImportanceOf (EmitterDistanceSampler (scene, outray)) distance =
        sampleImportanceOf distsampler distance
      where distsampler = getEmitterDistanceSampler scene outray

  getEmitterDistanceSampler scene outray = UniformDepthDistanceSampler (scene, outray, depthclosure, pointproperty) where
    depthclosure  = probeEmissivityClosure
    pointproperty = emissivityAt

  newtype ScattererDistanceSampler = ScattererDistanceSampler (Scene,Ray)
  instance Show ScattererDistanceSampler where
    show (ScattererDistanceSampler (scene, Ray origin dir)) =
      "ScattererDistanceSampler @" ++ showVector3 origin ++ show dir ++ " in Scene: " ++ show scene
  instance Sampleable ScattererDistanceSampler Distance where
    sampleWithImportanceFrom (ScattererDistanceSampler (scene, outray)) = do
        sampleddistance <- sampleWithImportanceFrom distsampler
        let distance = sampledValue sampleddistance
            endpoint = outray `followFor` distance
            endpointcontribution = scene `scatteringAt` endpoint
            distanceimportance = sampledImportance sampleddistance
            importance = distanceimportance * endpointcontribution
        return $ distance `withImportance` importance
      where distsampler = getScattererDistanceSampler scene outray

    sampleProbabilityOf (ScattererDistanceSampler (scene, outray)) distance =
        sampleProbabilityOf distsampler distance
      where distsampler = getScattererDistanceSampler scene outray

    sampleFrom (ScattererDistanceSampler (scene, outray)) =
        sampleFrom distsampler
      where distsampler = getScattererDistanceSampler scene outray

    sampleContributionOf (ScattererDistanceSampler (scene, outray)) distance = contribution
      where contribution = endpointcontribution * edgecontribution
            endpointcontribution = scene `scatteringAt` endpoint
            endpoint = outray `followFor` distance
            edgecontribution = sampleContributionOf distsampler distance
            distsampler = getScattererDistanceSampler scene outray

    sampleImportanceOf (ScattererDistanceSampler (scene, outray)) distance =
        sampleImportanceOf distsampler distance
      where distsampler = getScattererDistanceSampler scene outray

  getScattererDistanceSampler scene outray = UniformAvailableAttenuationDistanceSampler (scene, outray, depthclosure, pointproperty) where
    depthclosure  = probeExtinctionClosure
    pointproperty = extinctionAt


  type DistanceSamplerParameters = (Scene,
                                    Ray,
                                    Scene -> Ray -> (Double, Double) -> ProbeResult,
                                    Scene -> Point -> Double)

  newtype UniformDepthDistanceSampler = UniformDepthDistanceSampler DistanceSamplerParameters
  instance Sampleable UniformDepthDistanceSampler Distance where
    sampleProbabilityOf (UniformDepthDistanceSampler (scene,outray,depthclosure,pointproperty)) distance
      | endpointvalue==0 = 0
      | otherwise = endpointvalue / totaldepth
      where endpointvalue = pointproperty scene endpoint
            endpoint = outray `followFor` distance
            totaldepth = fromMaybe (error "UniformDepthDistanceSampler.sampleProbabilityOf: totaldepth") $ getProbeResultDepth totaldepthproberesult
            totaldepthproberesult = depthclosure scene outray (infinity,infinity)

    sampleFrom (UniformDepthDistanceSampler (scene,outray,depthclosure,pointproperty))
      | totaldepth==0 = fail "no material ahead to sample a distance from."
      | otherwise = do u1 <- lift getCoord
                       let randomdepth = totaldepth * u1
                           proberesult = probeMedia (infinity, randomdepth)
                           distance = fromMaybe (error "UniformDepthDistanceSampler.sampleFrom: distance") . getProbeResultDist $ proberesult
                           mindist = 1e-14 -- avoids 0div-errors because of identical points
                       return $ max mindist distance
      where totaldepth = fromMaybe (error "UniformDepthDistanceSampler.sampleFrom: totaldepth") $ getProbeResultDepth totaldepthproberesult
            totaldepthproberesult = probeMedia (infinity, infinity)
            probeMedia = depthclosure scene outray

    sampleContributionOf (UniformDepthDistanceSampler (scene,outray,depthclosure,pointproperty)) distance =
        (exp (-opticaldepth)) -- /distance^2 --cancels with geometric term in RecursivePointSampler probability
      where opticaldepth = fromMaybe (error "UniformDepthDistanceSampler.sampleContributionOf: opticaldepth") . getProbeResultDepth $ probeExtinction scene outray distance infinity


  newtype UniformAttenuationDistanceSampleable = UniformAttenuationDistanceSampleable DistanceSamplerParameters
  instance Sampleable UniformAttenuationDistanceSampleable Distance where
    sampleProbabilityOf (UniformAttenuationDistanceSampleable (scene,outray,depthclosure,pointproperty)) distance =
      let endpoint = outray `followFor` distance
          depth = fromMaybe (error "UniformAttenuationDistanceSampleable.sampleProbabilityOf: depth") . getProbeResultDepth $ depthclosure scene outray (distance, infinity)
          endpointvalue = pointproperty scene endpoint
      in endpointvalue * (exp (-depth))

    sampleFrom (UniformAttenuationDistanceSampleable (scene,outray,depthclosure,pointproperty)) = do
      u1 <- lift getCoord
      let depth = negate $ log (u1::Double)
          proberesult = depthclosure scene outray (infinity,depth)
          mdistance = getProbeResultDist proberesult
      case mdistance of
        Nothing       -> fail "depth overshoot"
        Just distance -> return distance

    sampleContributionOf (UniformAttenuationDistanceSampleable (scene,outray,depthclosure,pointproperty)) distance =
        (exp (-opticaldepth)) -- /distance^2 --cancels with geometric term in RecursivePointSampler probability
      where opticaldepth = fromMaybe (error "UniformAttenuationDistanceSampleable.sampleContributionOf: opticaldepth") . getProbeResultDepth $ probeExtinction scene outray distance infinity

  newtype UniformAvailableAttenuationDistanceSampler = UniformAvailableAttenuationDistanceSampler DistanceSamplerParameters
  instance Sampleable UniformAvailableAttenuationDistanceSampler Distance where
    sampleWithImportanceFrom (UniformAvailableAttenuationDistanceSampler
                               (scene,outray,depthclosure,pointproperty))
      | totaldepth==0 = fail "no material ahead to sample a distance from."
      | otherwise = do
          u1 <- lift getCoord
          let absorptionatinfinity = (1 - (exp (-totaldepth)))
              endpointdepth = negate $ log (1 - absorptionatinfinity * (u1::Double))
              proberesult = probeMedia (infinity, endpointdepth)
              distance = fromMaybe (error distanceerrormsg) . getProbeResultDist $ proberesult
              distanceerrormsg = "UniformAvailableAttenuationDistanceSampler.sampleWithImportanceFrom: distance totaldepth=" ++
                                 show totaldepth ++ " endpointdepth=" ++ show endpointdepth
              opticaldepth = fromMaybe (error "UniformAvailableAttenuationDistanceSampler.sampleWithImportanceFrom: opticaldepth") .
                             getProbeResultDepth $ probeExtinction scene outray distance infinity
              contribution = exp (-opticaldepth)
              endpoint = outray `followFor` distance
              endpointvalue = pointproperty scene endpoint
              probability = endpointvalue * (exp (-endpointdepth)) / absorptionatinfinity
              importance = contribution / probability
              mindist = 1e-14 -- avoids 0div-errors because of identical points
          return $ max mindist distance `withImportance` importance
      where totaldepth = fromMaybe (error "UniformAvailableAttenuationDistanceSampler.sampleWithImportanceFrom: totaldepth") . getProbeResultDepth $ probeMedia (infinity, infinity)
            probeMedia = depthclosure scene outray

    sampleProbabilityOf (UniformAvailableAttenuationDistanceSampler
                          (scene,outray,depthclosure,pointproperty) )
                        distance
      | endpointvalue==0 = 0
      | otherwise        = endpointvalue * (exp (-endpointdepth)) / absorptionatinfinity
      where absorptionatinfinity = (1 - (exp (-totaldepth)))
            endpointvalue = pointproperty scene endpoint
            endpoint = outray `followFor` distance
            endpointdepth = fromMaybe (error "UniformAvailableAttenuationDistanceSampler.sampleProbabilityOf: endpointdepth") . getProbeResultDepth $ probeMedia (distance, infinity)
            totaldepth    = fromMaybe (error "UniformAvailableAttenuationDistanceSampler.sampleProbabilityOf: totaldepth") . getProbeResultDepth $ probeMedia (infinity, infinity)
            probeMedia = depthclosure scene outray

    sampleFrom (UniformAvailableAttenuationDistanceSampler
                       (scene,outray,depthclosure,pointproperty))
      | totaldepth==0 = fail "no material ahead to sample a distance from."
      | otherwise = do u1 <- lift getCoord
                       let absorptionatinfinity = (1 - (exp (-totaldepth)))
                           randomdepth = negate $ log (1 - absorptionatinfinity * (u1::Double))
                           proberesult = probeMedia (infinity, randomdepth)
                           distance = fromMaybe (error distanceerrormsg) . getProbeResultDist $ proberesult
                           distanceerrormsg = "UniformAvailableAttenuationDistanceSampler.sampleFrom: distance totaldepth=" ++
                                              show totaldepth ++ " randomdepth=" ++ show randomdepth
                           mindist = 1e-14 -- avoids 0div-errors because of identical points
                       return $ max mindist distance
      where totaldepth = fromMaybe (error "UniformAvailableAttenuationDistanceSampler.sampleFrom: totaldepth") . getProbeResultDepth $ probeMedia (infinity, infinity)
            probeMedia = depthclosure scene outray

    sampleContributionOf (UniformAvailableAttenuationDistanceSampler (scene,outray,depthclosure,pointproperty)) distance =
        (exp (-opticaldepth)) -- /distance^2 --cancels with geometric term in RecursivePointSampler probability
      where opticaldepth = fromMaybe (error "UniformAvailableAttenuationDistanceSampler.sampleContributionOf: opticaldepth") . getProbeResultDepth $ probeExtinction scene outray distance infinity


  -- let s=SensorDistanceSampler (standardScene 0.7, fromTwoPoints (Vector3 0 0 0) (Vector3 0 0 (-1)))
  -- runUCToMaybeSampled (sampleWithImportanceFrom s) (toStream 13) :: MaybeSampled Distance
  {--
  newtype UniformAttenuationDistanceSampleable = UniformAttenuationDistanceSampleable DistanceSamplerParameters
  instance Sampleable UniformAttenuationDistanceSampleable (Maybe Double) where
    randomSampleFrom (UniformAttenuationDistanceSampleable (entities,materialproperty,ray)) g = do
      u1 <- uniform g
      let depth = negate $ log (u1::Double)
          proberesult = probePropertyOfEntitiesWithRay materialproperty entities ray infinity depth
          distance = getProbeResultDist proberesult
      return distance

    sampleProbabilityOf (UniformAttenuationDistanceSampleable (entities,
                                                              materialproperty,
                                                              Ray origin (Direction direction)
                                                              ))
                        (Just distance) =
      let endpoint = origin + distance |* direction
          depth = depthOfBetween materialproperty entities origin endpoint
          endpointvalue = propertyAt materialproperty entities endpoint
      in endpointvalue * (exp (-depth))
    sampleProbabilityOf _ Nothing = samplingNothingError "UniformAttenuationDistanceSampleable"

  newtype UniformAttenuation2Sampleable = UniformAttenuation2Sampleable DistanceSamplerParameters
  instance Sampleable UniformAttenuation2Sampleable (Maybe Double) where
    randomSampleFrom (UniformAttenuation2Sampleable (entities,materialproperty,ray)) g
      | totaldepth==0 = return Nothing
      | otherwise = do u1 <- uniform g
                       let absorptionatinfinity = (1 - (exp (-totaldepth)))
                           randomdepth = negate $ log (1 - absorptionatinfinity * (u1::Double))
                           proberesult = probeMedia (infinity, randomdepth)
                           distance = getProbeResultDist proberesult
                           mindist = 1e-14 -- avoids 0div-errors because of identical points
                       return $ (max mindist) `fmap` distance
      where totaldepth = fromJust $ getProbeResultDepth totaldepthproberesult
            totaldepthproberesult = probeMedia (infinity, infinity)
            probeMedia = probePropertyOfEntitiesWithRayClosure materialproperty entities ray

    sampleProbabilityOf (UniformAttenuation2Sampleable (entities,
                                                        materialproperty,
                                                        ray@(Ray origin (Direction direction))
                                                        ))
                        (Just distance)
      = --trace ("totaldepth="++show totaldepth++" ,endpointvalue="++show endpointvalue) $
        endpointvalue * (exp (-endpointdepth)) / absorptionatinfinity
        where absorptionatinfinity = (1 - (exp (-totaldepth)))
              endpointvalue = propertyAt materialproperty entities endpoint
              endpoint = origin + distance |* direction
              endpointdepth = fromJust . getProbeResultDepth $ probeMedia (distance, infinity)
              totaldepth    = fromJust . getProbeResultDepth $ probeMedia (infinity, infinity)
              probeMedia = probePropertyOfEntitiesWithRayClosure materialproperty entities ray
    sampleProbabilityOf _ Nothing = samplingNothingError "UniformAttenuation2Sampleable"

  newtype UniformDepthDistanceSampleable = UniformDepthDistanceSampleable DistanceSamplerParameters
  instance Sampleable UniformDepthDistanceSampleable (Maybe Double) where
    randomSampleFrom (UniformDepthDistanceSampleable (entities,materialproperty,ray)) g
      | totaldepth==0 = return Nothing
      | otherwise = do u1 <- uniform g
                       let randomdepth = totaldepth * (u1::Double)
                           proberesult = probeMedia (infinity, randomdepth)
                           distance = getProbeResultDist proberesult
                           mindist = 1e-14 -- avoids 0div-errors because of identical points
                       return $ (max mindist) `fmap` distance
      where totaldepth = fromJust $ getProbeResultDepth totaldepthproberesult
            totaldepthproberesult = probeMedia (infinity, infinity)
            probeMedia = probePropertyOfEntitiesWithRayClosure materialproperty entities ray

    sampleProbabilityOf (UniformDepthDistanceSampleable (entities,
                                                         materialproperty,
                                                         ray@(Ray origin (Direction direction))
                                                         ))
                        (Just distance)
      = --trace ("totaldepth="++show totaldepth++" ,endpointvalue="++show endpointvalue) $
        endpointvalue / totaldepth
        where endpointvalue = propertyAt materialproperty entities endpoint
              endpoint = origin + distance |* direction
              totaldepth = fromJust $ getProbeResultDepth totaldepthproberesult
              totaldepthproberesult = probePropertyOfEntitiesWithRay materialproperty entities ray infinity infinity
    sampleProbabilityOf _ Nothing = samplingNothingError "UniformDepthDistanceSampleable"
  --}