{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.Path.PathSamplerAtoms where
  import Data.ACVector
  import PIRaTE.SpatialTypes
  {--import PIRaTE.UtilityFunctions (infinity,edgeMap)
  import PIRaTE.Scene.Confineable
  import PIRaTE.Scene.Container
  import PIRaTE.Scene.Container.Sphere
  import PIRaTE.Scene.Container.Box
  import PIRaTE.Scene.PhaseFunction
  import PIRaTE.Scene.PhaseFunction.Isotropic
  import PIRaTE.Scene.PhaseFunction.ZCone
  import PIRaTE.Scene.Texture
  import PIRaTE.Scene.Material
  import PIRaTE.Scene.Sensor--}
  import PIRaTE.Scene.Scene
  import PIRaTE.MonteCarlo.Sampled
  import Control.Applicative
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

  {--
  -- Distance Samplers
  data DistanceSampler = forall s . (Sampleable s (Maybe Double)) => DistanceSampler s

  instance Sampleable DistanceSampler (Maybe Double) where
    randomSampleFrom (DistanceSampler ds) = randomSampleFrom ds
    sampleProbabilityOf (DistanceSampler ds) = sampleProbabilityOf ds

  newtype SensationDistanceSampler = SensationDistanceSampler (Scene,Point,Direction)
  instance Show SensationDistanceSampler where
    show (SensationDistanceSampler (scene,origin,Direction dir)) =
      "SensationDistanceSampler @" ++ showVector3 origin ++
      "->@" ++ showVector3 dir ++
      "in Scene: " ++ show scene
  instance Sampleable SensationDistanceSampler (Maybe Double) where
    randomSampleFrom (SensationDistanceSampler (scene,origin,direction)) g =
      randomSampleFrom distsampler g
      where distsampler = getSensationDistanceSampler scene origin direction
    {-# INLINE randomSampleFrom #-}

    sampleProbabilityOf (SensationDistanceSampler (scene,origin,direction)) distance = 
      sampleProbabilityOf distsampler distance
      where distsampler = getSensationDistanceSampler scene origin direction
    {-# INLINE sampleProbabilityOf #-}
  
  getSensationDistanceSampler scene origin direction = UniformDepthDistanceSampleable (sensors, property, ray) where
    sensors = sceneSensors scene
    property = materialSensitivity
    ray = Ray origin direction
  {-# INLINE getSensationDistanceSampler #-}

  newtype EmissionDistanceSampler = EmissionDistanceSampler (Scene,Point,Direction)
  instance Show EmissionDistanceSampler where
    show (EmissionDistanceSampler (scene,origin,Direction dir)) =
      "EmissionDistanceSampler @" ++ showVector3 origin ++
      "->@" ++ showVector3 dir ++
      "in Scene: " ++ show scene
  instance Sampleable EmissionDistanceSampler (Maybe Double) where
    randomSampleFrom (EmissionDistanceSampler (scene,origin,direction)) g =
      randomSampleFrom distsampler g
      where distsampler = getEmissionDistanceSampler scene origin direction
    {-# INLINE randomSampleFrom #-}

    sampleProbabilityOf (EmissionDistanceSampler (scene,origin,direction)) distance = 
      sampleProbabilityOf distsampler distance
      where distsampler = getEmissionDistanceSampler scene origin direction
    {-# INLINE sampleProbabilityOf #-}

  getEmissionDistanceSampler scene origin direction = UniformDepthDistanceSampleable (emitters, property, ray) where
    emitters = sceneEmitters scene
    property = materialEmissivity
    ray = Ray origin direction
  {-# INLINE getEmissionDistanceSampler #-}

  newtype ScatteringDistanceSampler = ScatteringDistanceSampler (Scene,Point,Direction)
  instance Show ScatteringDistanceSampler where
    show (ScatteringDistanceSampler (scene,origin,Direction dir)) =
      "ScatteringDistanceSampler @" ++ showVector3 origin ++
      "->@" ++ showVector3 dir ++
      "in Scene: " ++ show scene
  instance Sampleable ScatteringDistanceSampler (Maybe Double) where
    randomSampleFrom (ScatteringDistanceSampler (scene,origin,direction)) g =
      randomSampleFrom distsampler g
      where distsampler = getScatteringDistanceSampler scene origin direction
    {-# INLINE randomSampleFrom #-}

    sampleProbabilityOf (ScatteringDistanceSampler (scene,origin,direction)) distance = 
      sampleProbabilityOf distsampler distance
      where distsampler = getScatteringDistanceSampler scene origin direction
    {-# INLINE sampleProbabilityOf #-}

  getScatteringDistanceSampler scene origin direction = UniformAttenuationDistanceSampleable (scatterers, property, ray) where
    scatterers = sceneScatterers scene
    property = materialScattering
    ray = Ray origin direction
  {-# INLINE getScatteringDistanceSampler #-}

  type DistanceSamplerParameters = ([Entity], Material -> Texture Double, Ray)

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