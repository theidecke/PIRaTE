{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.Path.PathGenerators where
  import Data.ACVector
  --import Data.Maybe (fromJust)
  import PIRaTE.UtilityFunctions (normsq)
  import PIRaTE.SpatialTypes
  import PIRaTE.Scene.Scene
  import PIRaTE.MonteCarlo.Sampled
  import PIRaTE.Path.PathSamplerAtoms

  data NodeType = Emi | Sca | Sen deriving Show
  type TypedRay   = (  Ray, NodeType)
  type TypedPoint = (Point, NodeType)

  newtype StupidPathGenerator = StupidPathGenerator (Scene,Double) deriving Show
  instance Sampleable StupidPathGenerator MLTState where
    sampleWithImportanceFrom (StupidPathGenerator (scene, growprobability)) = do
      sampledemissionpoint  <- sampleWithImportanceFrom (EmissionPointSampler  scene)
      sampledsensationpoint <- sampleWithImportanceFrom (SensationPointSampler scene)
      let emissionpoint  = sampledValue sampledemissionpoint
          sensationpoint = sampledValue sampledsensationpoint
          emissionimportance  = sampledImportance sampledemissionpoint
          sensationimportance = sampledImportance sampledsensationpoint
          lightpath  = [emissionpoint]  -- reverse order! r2:r1:r0:[]
          sensorpath = [sensationpoint] -- reverse order! r3:r4:r5:[]
          pathvalue = (reverse lightpath) ++ sensorpath
          connectioncontribution = getConnectionContribution scene (Ray emissionpoint (error "error: undefined1"), Emi) (Ray sensationpoint (error "error: undefined2"), Sen)
          pathimportance = emissionimportance * connectioncontribution * sensationimportance
      return $ (fromPath pathvalue) `withImportance` pathimportance

    sampleProbabilityOf _ _ = (error "error: undefined3")

  --let pg = StupidPathGenerator (standardScene 0.7,0.5)
  --filter (\x->sampledImportance x > 0) . fromJust $ runUCToMaybeSampled (replicateM 10 . sampleWithImportanceFrom $ pg) (toStream 13) :: [Sampled MLTState]

  newtype SimplePathGenerator = SimplePathGenerator (Scene,Double) deriving Show
  instance Sampleable SimplePathGenerator MLTState where
    sampleWithImportanceFrom (SimplePathGenerator (scene, growprobability)) = do
      sampledsensationpoint <- sampleWithImportanceFrom (SensationPointSampler scene)
      let sensationpoint = sampledValue sampledsensationpoint
          sensorinray    = Ray sensationpoint (error "error: undefined4")
      sampledemissionpoint <- sampleWithImportanceFrom (RaycastingPointSampler (scene,(sensorinray,Sen),Emi))
      let (Ray emissionpoint negemissionoutdir,_) = (sampledValue sampledemissionpoint)::TypedRay
          sensorpath = [sensationpoint] -- reverse order! r1:r2:r3:[]
          pathvalue = emissionpoint : sensorpath
          emissionoutdir = negate `appliedToDirection` negemissionoutdir
          emissiondirimportance = getScatteringContribution scene Emi (Ray emissionpoint (error "error: undefined5")) emissionoutdir
          sensationpointimportance = sampledImportance sampledsensationpoint
          emissionpointimportance  = sampledImportance sampledemissionpoint
          pathimportance = sensationpointimportance * emissionpointimportance * emissiondirimportance
      return $ (fromPath pathvalue) `withImportance` pathimportance

    sampleProbabilityOf _ _ = (error "error: undefined6")


  newtype SimplePathtracerPathGenerator = SimplePathtracerPathGenerator (Scene) deriving Show
  instance Sampleable SimplePathtracerPathGenerator MLTState where
    sampleWithImportanceFrom (SimplePathtracerPathGenerator scene) = do
      sampledemissionpoint  <- sampleWithImportanceFrom (EmissionPointSampler  scene)
      sampledsensationpoint <- sampleWithImportanceFrom (SensationPointSampler scene)
      let emissionpoint  = sampledValue sampledemissionpoint
          sensationpoint = sampledValue sampledsensationpoint
          sensorinray    = Ray sensationpoint (error "error: undefined7")
          typedsensorinray = (sensorinray,Sen)
      sampledscatterinrays <- samplePointRecursively scene typedsensorinray
      let typedscatterinrays    = sampledValue sampledscatterinrays
          scatterinrays         = map fst typedscatterinrays
          -- finish path by connecting with the emissionpoint
          lasttypedinray = last (typedsensorinray : typedscatterinrays)
          scatterpoints = map rayOrigin scatterinrays
          pathvalue = emissionpoint : reverse (sensationpoint : scatterpoints)
          sensationpointimportance = sampledImportance sampledsensationpoint
          emissionpointimportance  = sampledImportance sampledemissionpoint
          scatterinraysimportance  = sampledImportance sampledscatterinrays
          connectioncontribution = getConnectionContribution scene (Ray emissionpoint (error "error: undefined8"), Emi) lasttypedinray
          pathimportance = sensationpointimportance * scatterinraysimportance * connectioncontribution * emissionpointimportance
      return $ (fromPath pathvalue) `withImportance` pathimportance

    sampleProbabilityOf _ _ = (error "error: undefined9")

  samplePointRecursively :: Scene -> TypedRay -> UCToMaybeSampled [TypedRay]
  samplePointRecursively scene typedinray1 = do
    growdiceroll <- lift getCoord -- sample another scatteringpoint?
    let scatterpoint = rayOrigin . fst $ typedinray1
        albedo = scene `albedoAt` scatterpoint
        growprobability = case snd typedinray1 of
          Sca -> 0.6 --min 0.9 albedo
          _   -> 0.9 -- we start on an emitter or sensor
    if growdiceroll < growprobability
      then do -- yes, sample a new scatteringpoint
        sampledscatterinray <- sampleWithImportanceFrom (RaycastingPointSampler (scene,typedinray1,Sca))
        let scatterinray           = (sampledValue sampledscatterinray)::TypedRay
            scatterinrayimportance = sampledImportance sampledscatterinray
        sampledfutureinrays <- samplePointRecursively scene scatterinray
        let futureinrays = sampledValue sampledfutureinrays
            futureimportance = sampledImportance sampledfutureinrays
            importance = futureimportance * scatterinrayimportance / growprobability
        return $ (scatterinray:futureinrays) `withImportance` importance
      else do -- no, return an empty list
        return $ [] `withProbability` (1 - growprobability)

  newtype RaycastingPointSampler = RaycastingPointSampler (Scene, TypedRay, NodeType)
  instance Sampleable RaycastingPointSampler TypedRay where
    sampleWithImportanceFrom (RaycastingPointSampler (scene, (inray1, nodetype1), nodetype2)) = do
      let outdirsampler = (directionSamplerFactory nodetype1) (scene, inray1)
      sampledoutdir1 <- sampleWithImportanceFrom outdirsampler
      let outdir1           = sampledValue      sampledoutdir1
          outdir1importance = sampledImportance sampledoutdir1
          outray1 = Ray (rayOrigin inray1) outdir1
          distancesampler = (distanceSamplerFactory nodetype2) (scene, outray1)
      sampleddistance <- sampleWithImportanceFrom distancesampler
      let distance           = sampledValue sampleddistance
          distanceimportance = sampledImportance sampleddistance
          inray2 = Ray (outray1 `followFor` distance) (rayDirection outray1)
          inray2importance = outdir1importance * distanceimportance
      return $ (inray2,nodetype2) `withImportance` inray2importance

    sampleProbabilityOf _ _ = (error "error: undefined10")


  getConnectionContribution :: Scene -> TypedRay -> TypedRay -> Double
  getConnectionContribution scene (inray1,nodetype1) (inray2,nodetype2) =
      scacontrib1 * edgecontrib * scacontrib2
    where scacontrib1 = getScatteringContribution scene nodetype1 inray1 outdir1
          scacontrib2 = getScatteringContribution scene nodetype2 inray2 outdir2
          edgecontrib = (exp (-opticaldepth))/distancesq
          opticaldepth = opticalDepthBetween scene p1 p2
          distancesq = normsq edge12
          outdir2 = negate `appliedToDirection` outdir1
          outdir1 = fromEdge edge12
          edge12 = p2 - p1
          p1 = rayOrigin inray1
          p2 = rayOrigin inray2

  getScatteringContribution scene nodetype inray outdir =
      sampleProbabilityOf directionsampler outdir
    where directionsampler = (directionSamplerFactory nodetype) (scene,inray)

  directionSamplerFactory nodetype = case nodetype of
    Emi -> DirectionSampler . EmissionDirectionSampler
    Sca -> DirectionSampler . ScatteringDirectionSampler
    Sen -> DirectionSampler . SensationDirectionSampler

  distanceSamplerFactory nodetype = case nodetype of
    Emi -> DistanceSampler . EmitterDistanceSampler
    Sca -> DistanceSampler . ScattererDistanceSampler
    Sen -> DistanceSampler . SensorDistanceSampler
