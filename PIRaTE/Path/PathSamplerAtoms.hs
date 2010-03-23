samplingNothingError name = error $ "don't know " ++ name ++ " probability of sampling Nothing."

--data PointSampler = forall s . (Sampleable s (Maybe Point)) => PointSampler s
newtype SensationPointSampler = SensationPointSampler Scene
instance Show SensationPointSampler where
  show (SensationPointSampler scene) = "SensationPointSampler in Scene: " ++ show scene
instance Sampleable SensationPointSampler (Maybe Point) where
  randomSampleFrom (SensationPointSampler scene) g
    | null sensors = return Nothing
    | otherwise = do entity <- randomSampleFrom sensors g
                     let container = entityContainer entity
                     origin <- randomSampleFrom container g
                     return (Just origin)
    where sensors = sceneSensors scene

  sampleProbabilityOf (SensationPointSampler scene) (Just origin)
    | null sensors = 0
    | otherwise = sum [(sampleProbabilityOf (entityContainer sensor) origin) *
                       (sampleProbabilityOf sensors sensor) | sensor <- sensors]
    where sensors = sceneSensors scene `containing` origin
  sampleProbabilityOf (SensationPointSampler scene) Nothing =
    samplingNothingError "SensationPointSampler"

instance Arbitrary SensationPointSampler where
  arbitrary = SensationPointSampler `fmap` arbitrary

prop_SensationPointSampler_nonzeroProb :: SensationPointSampler -> Int -> Property
prop_SensationPointSampler_nonzeroProb sampler@(SensationPointSampler scene) seedint =
  isJust mpoint ==> f sampleprob
  where f | any (`contains` point) sensors = (>0)
          | otherwise                      = (==0)
        point = fromJust mpoint
        sampleprob = sampleProbabilityOf sampler mpoint
        mpoint = runRandomSampler sampler seedint
        sensors = map entityContainer $ sceneSensors scene


newtype EmissionPointSampler = EmissionPointSampler Scene
instance Show EmissionPointSampler where
  show (EmissionPointSampler scene) = "EmissionPointSampler in Scene: " ++ show scene
instance Sampleable EmissionPointSampler (Maybe Point) where
  randomSampleFrom (EmissionPointSampler scene) g
    | null emitters = return Nothing
    | otherwise = do entity <- randomSampleFrom emitters g
                     let container = entityContainer entity
                     origin <- randomSampleFrom container g
                     return (Just origin)
    where emitters = sceneEmitters scene

  sampleProbabilityOf (EmissionPointSampler scene) (Just origin)
    | null emitters = 0
    | otherwise = sum [(sampleProbabilityOf (entityContainer emitter) origin) *
                       (sampleProbabilityOf emitters emitter) | emitter <- emitters]
    where emitters = sceneEmitters scene `containing` origin
  sampleProbabilityOf (EmissionPointSampler scene) Nothing =
    samplingNothingError "EmissionPointSampler"

instance Arbitrary EmissionPointSampler where
  arbitrary = EmissionPointSampler `fmap` arbitrary

prop_EmissionPointSampler_nonzeroProb :: EmissionPointSampler -> Int -> Property
prop_EmissionPointSampler_nonzeroProb sampler@(EmissionPointSampler scene) seedint =
  isJust mpoint ==> f sampleprob
  where f | any (`contains` point) emitters = (>0)
          | otherwise                       = (==0)
        point = fromJust mpoint
        sampleprob = sampleProbabilityOf sampler mpoint
        mpoint = runRandomSampler sampler seedint
        emitters = map entityContainer $ sceneEmitters scene

newtype ScatteringPointSampler = ScatteringPointSampler Scene
instance Show ScatteringPointSampler where
  show (ScatteringPointSampler scene) = "ScatteringPointSampler in Scene: " ++ show scene
instance Sampleable ScatteringPointSampler (Maybe Point) where
  randomSampleFrom (ScatteringPointSampler scene) g
    | null scatterers = return Nothing
    | otherwise = do entity <- randomSampleFrom scatterers g
                     let container = entityContainer entity
                     origin <- randomSampleFrom container g
                     return (Just origin)
    where scatterers = sceneScatterers scene

  sampleProbabilityOf (ScatteringPointSampler scene) (Just origin)
    | null scatterers = 0
    | otherwise = sum [(sampleProbabilityOf (entityContainer scatterer) origin) *
                       (sampleProbabilityOf scatterers scatterer) | scatterer <- scatterers]
    where scatterers = sceneScatterers scene `containing` origin
  sampleProbabilityOf (ScatteringPointSampler scene) Nothing =
    samplingNothingError "ScatteringPointSampler"

instance Arbitrary ScatteringPointSampler where
  arbitrary = ScatteringPointSampler `fmap` arbitrary

prop_ScatteringPointSampler_nonzeroProb :: ScatteringPointSampler -> Int -> Property
prop_ScatteringPointSampler_nonzeroProb sampler@(ScatteringPointSampler scene) seedint =
  isJust mpoint ==> f sampleprob
  where f | any (`contains` point) scatterers = (>0)
          | otherwise                         = (==0)
        point = fromJust mpoint
        sampleprob = sampleProbabilityOf sampler mpoint
        mpoint = runRandomSampler sampler seedint
        scatterers = map entityContainer $ sceneScatterers scene
  
-- Direction Samplers
data DirectionSampler = forall s . (IsDirSampler s, Sampleable s (Maybe Direction)) => DirectionSampler s

instance Sampleable DirectionSampler (Maybe Direction) where
  randomSampleFrom (DirectionSampler ds) = randomSampleFrom ds
  sampleProbabilityOf (DirectionSampler ds) = sampleProbabilityOf ds

class IsDirSampler a where
  dirSamplerOrigin :: a -> Point

instance IsDirSampler DirectionSampler where dirSamplerOrigin (DirectionSampler ds)   = dirSamplerOrigin ds
instance IsDirSampler SensationDirectionSampler  where dirSamplerOrigin (SensationDirectionSampler  (_,origin))   = origin
instance IsDirSampler EmissionDirectionSampler   where dirSamplerOrigin (EmissionDirectionSampler   (_,origin))   = origin
instance IsDirSampler ScatteringDirectionSampler where dirSamplerOrigin (ScatteringDirectionSampler (_,origin,_)) = origin

newtype SensationDirectionSampler = SensationDirectionSampler (Scene, Point)
instance Show SensationDirectionSampler where
  show (SensationDirectionSampler (scene,origin)) = "SensationDirectionSampler @" ++ showVector3 origin ++
                                                    "in Scene: " ++ show scene
instance Sampleable SensationDirectionSampler (Maybe Direction) where
  randomSampleFrom (SensationDirectionSampler (scene,origin)) g
    | (scene `sensitivityAt` origin)==0 = return Nothing
    | otherwise = do direction <- randomSampleFrom (weightedsensors,origin) g
                     return (Just direction)
    where weightedsensors = scene `sensorDirectednessesAt` origin
          --TODO: remove double call to `containing` hidden in summedMaterialAt

  sampleProbabilityOf (SensationDirectionSampler (scene,origin)) (Just direction) =
    sampleProbabilityOf (weightedsensors, origin) direction
    where weightedsensors = scene `sensorDirectednessesAt` origin
  sampleProbabilityOf (SensationDirectionSampler (scene,origin)) Nothing =
    samplingNothingError "SensationDirectionSampler"

prop_SensationDirectionSampler_nonzeroProb :: Scene -> Int -> Property
prop_SensationDirectionSampler_nonzeroProb scene seedint =
  isJust mpoint && isJust mdir ==> sampleprob > 0
  where sampleprob = sampleProbabilityOf dirsampler mdir
        mdir = (runRandomSampler dirsampler seedint)::(Maybe Direction)
        dirsampler = SensationDirectionSampler (scene,point)
        point = fromJust mpoint
        mpoint = runRandomSampler pointsampler seedint
        pointsampler = SensationPointSampler scene

newtype EmissionDirectionSampler = EmissionDirectionSampler (Scene, Point)
instance Show EmissionDirectionSampler where
  show (EmissionDirectionSampler (scene,origin)) = "EmissionDirectionSampler @" ++ showVector3 origin ++
                                                   "in Scene: " ++ show scene
instance Sampleable EmissionDirectionSampler (Maybe Direction) where
  randomSampleFrom (EmissionDirectionSampler (scene,origin)) g
    | (scene `emissivityAt` origin)==0 = return Nothing
    | otherwise = do direction <- randomSampleFrom (weightedphasefunctions, Ray origin undefined) g
                     return (Just direction)
    where weightedphasefunctions = scene `emissionDirectednessesAt` origin
          
  sampleProbabilityOf (EmissionDirectionSampler (scene,origin)) (Just direction) =
    sampleProbabilityOf (weightedphasefunctions, Ray origin undefined) direction
    where weightedphasefunctions = scene `emissionDirectednessesAt` origin
  sampleProbabilityOf (EmissionDirectionSampler (scene,origin)) Nothing =
    samplingNothingError "EmissionDirectionSampler"

prop_EmissionDirectionSampler_nonzeroProb :: Scene -> Int -> Property
prop_EmissionDirectionSampler_nonzeroProb scene seedint =
  isJust mpoint && isJust mdir ==> sampleprob > 0
  where sampleprob = sampleProbabilityOf dirsampler mdir
        mdir = (runRandomSampler dirsampler seedint)::(Maybe Direction)
        dirsampler = EmissionDirectionSampler (scene,point)
        point = fromJust mpoint
        mpoint = runRandomSampler pointsampler seedint
        pointsampler = EmissionPointSampler scene


newtype ScatteringDirectionSampler = ScatteringDirectionSampler (Scene, Point, Direction)
instance Show ScatteringDirectionSampler where
  show (ScatteringDirectionSampler (scene,origin,Direction dir)) =
    "ScatteringDirectionSampler @" ++ showVector3 origin ++
    "->@" ++ showVector3 dir ++
    "in Scene: " ++ show scene
instance Sampleable ScatteringDirectionSampler (Maybe Direction) where
  randomSampleFrom (ScatteringDirectionSampler (scene,origin,win)) g
    | (scene `scatteringAt` origin)==0 = return Nothing
    | otherwise = do wout <- randomSampleFrom (weightedphasefunctions, Ray origin win) g
                     return (Just wout)
    where weightedphasefunctions = scene `scatterPhaseFunctionsAt` origin
          
  sampleProbabilityOf (ScatteringDirectionSampler (scene,origin,win)) (Just wout) = 
    sampleProbabilityOf (weightedphasefunctions, Ray origin win) wout where
      weightedphasefunctions = scene `scatterPhaseFunctionsAt` origin
  sampleProbabilityOf (ScatteringDirectionSampler (scene,origin,win)) Nothing =
    samplingNothingError "ScatteringDirectionSampler"

prop_ScatteringDirectionSampler_nonzeroProb :: Scene -> Int -> Property
prop_ScatteringDirectionSampler_nonzeroProb scene seedint =
  isJust mpoint && isJust mdir ==> sampleprob > 0
  where sampleprob = sampleProbabilityOf dirsampler mdir
        mdir = (runRandomSampler dirsampler (seedint+2))::(Maybe Direction)
        dirsampler = ScatteringDirectionSampler (scene,point,olddir)
        olddir = (runRandomSampler olddirsampler (seedint+1))::Direction
        olddirsampler = (Isotropic,undefined::Ray)
        point = fromJust mpoint
        mpoint = runRandomSampler pointsampler seedint
        pointsampler = ScatteringPointSampler scene

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

prop_SensationDistanceSampler_nonzeroProb :: Scene -> Int -> Property
prop_SensationDistanceSampler_nonzeroProb scene seedint =
  isJust mdist ==> sampleprob > 0
  where sampleprob = sampleProbabilityOf distsampler mdist
        mdist = (runRandomSampler distsampler (seedint+2))::(Maybe Double)
        dir   = (runRandomSampler  dirsampler (seedint+1))::Direction
        point  = runRandomSampler pointsampler seedint
        distsampler = SensationDistanceSampler (scene,point,dir)
        dirsampler = (Isotropic,undefined::Ray)
        pointsampler = Exponential3DPointSampler 1.0

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

prop_EmissionDistanceSampler_nonzeroProb :: Scene -> Int -> Property
prop_EmissionDistanceSampler_nonzeroProb scene seedint =
  isJust mdist ==> sampleprob > 0
  where sampleprob = sampleProbabilityOf distsampler mdist
        mdist = (runRandomSampler distsampler (seedint+2))::(Maybe Double)
        dir   = (runRandomSampler  dirsampler (seedint+1))::Direction
        point  = runRandomSampler pointsampler seedint
        distsampler = EmissionDistanceSampler (scene,point,dir)
        dirsampler = (Isotropic,undefined::Ray)
        pointsampler = Exponential3DPointSampler 1.0

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

prop_ScatteringDistanceSampler_nonzeroProb :: Scene -> Int -> Property
prop_ScatteringDistanceSampler_nonzeroProb scene seedint =
  isJust mdist ==> sampleprob > 0
  where sampleprob = sampleProbabilityOf distsampler mdist
        mdist = (runRandomSampler distsampler (seedint+2))::(Maybe Double)
        dir   = (runRandomSampler  dirsampler (seedint+1))::Direction
        point  = runRandomSampler pointsampler seedint
        distsampler = ScatteringDistanceSampler (scene,point,dir)
        dirsampler = (Isotropic,undefined::Ray)
        pointsampler = Exponential3DPointSampler 1.0

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
