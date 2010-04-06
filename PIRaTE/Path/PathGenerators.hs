{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.Path.PathGenerators where
  import Data.ACVector
  import Data.Maybe (fromJust)
  import PIRaTE.UtilityFunctions (normsq,infinity)
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
          firstgrowprob = 0.7
      sampledscatterinrays <- samplePointRecursively scene firstgrowprob typedsensorinray
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


  newtype DirectLightPathtracerPathGenerator = DirectLightPathtracerPathGenerator (Scene) deriving Show
  instance Sampleable DirectLightPathtracerPathGenerator MLTState where
    sampleWithImportanceFrom (DirectLightPathtracerPathGenerator scene) = do
      sampledsensationpoint <- sampleWithImportanceFrom (SensationPointSampler scene)
      let sensationpoint = sampledValue sampledsensationpoint
      sensationdir <- sampleFrom (SensationDirectionSampler (scene, Ray sensationpoint (error "DirectLightPathtracerPathGenerator sensationdir")))
      let sensorray = Ray sensationpoint sensationdir
          emissionproberesult = probeEmission scene sensorray infinity infinity
          lightsourceahead = (>0) . fromJust . getProbeResultDepth $ emissionproberesult
      let directlightprobability = if lightsourceahead then 0.95 else 0.05
      directlightdiceroll <- lift getCoord
      if directlightdiceroll < directlightprobability
        then do
          sampleddirectlightpath <- dlptDirectLight scene sampledsensationpoint
          return $ sampleddirectlightpath `scaleImportanceWith` (1/directlightprobability)
        else do
          sampledscatterlightpath <- dlptScatteredLight scene sampledsensationpoint
          let scatterlightprobability = 1 - directlightprobability
          return $ sampledscatterlightpath `scaleImportanceWith` (1/scatterlightprobability)

    sampleProbabilityOf _ _ = (error "error: undefined9b")


  dlptScatteredLight scene sampledsensationpoint = do
    sampledemissionpoint  <- sampleWithImportanceFrom (EmissionPointSampler  scene)
    let emissionpoint  = sampledValue sampledemissionpoint
        sensationpoint = sampledValue sampledsensationpoint
        sensorinray    = Ray sensationpoint (error "error: undefined7")
        typedsensorinray = (sensorinray,Sen)
        firstgrowprob = 1 -- we want at least one scatterpoint to ensure there aren't multiple ways to construct direct lighting paths
    sampledscatterinrays <- samplePointRecursively scene firstgrowprob typedsensorinray
    let typedscatterinrays    = sampledValue sampledscatterinrays
        scatterinrays         = map fst typedscatterinrays
        -- finish path by connecting with the emissionpoint
        lasttypedinray = last (typedsensorinray : typedscatterinrays)
        scatterpoints = map rayOrigin scatterinrays
        pathvalue = emissionpoint : reverse (sensationpoint : scatterpoints)
        sensationpointimportance = sampledImportance sampledsensationpoint
        emissionpointimportance  = sampledImportance sampledemissionpoint
        scatterinraysimportance  = sampledImportance sampledscatterinrays
        connectioncontribution = getConnectionContribution scene (Ray emissionpoint (error "error: undefined8b"), Emi) lasttypedinray
        pathimportance = sensationpointimportance * scatterinraysimportance * connectioncontribution * emissionpointimportance
    return $ (fromPath pathvalue) `withImportance` pathimportance

  dlptDirectLight scene sampledsensationpoint = do
    let sensationpoint = sampledValue sampledsensationpoint
        sensorinray    = Ray sensationpoint (error "error: undefined4b")
    sampledemissionpoint <- sampleWithImportanceFrom (RaycastingPointSampler (scene,(sensorinray,Sen),Emi))
    let (Ray emissionpoint negemissionoutdir,_) = (sampledValue sampledemissionpoint)::TypedRay
        pathvalue = [emissionpoint,sensationpoint]
        emissionoutdir = negate `appliedToDirection` negemissionoutdir
        emissiondirimportance = getScatteringContribution scene Emi (Ray emissionpoint (error "error: undefined5b")) emissionoutdir
        sensationpointimportance = sampledImportance sampledsensationpoint
        emissionpointimportance  = sampledImportance sampledemissionpoint
        pathimportance = sensationpointimportance * emissionpointimportance * emissiondirimportance
    return $ (fromPath pathvalue) `withImportance` pathimportance



  samplePointRecursively :: Scene -> Double -> TypedRay -> UCToMaybeSampled [TypedRay]
  samplePointRecursively scene growprobability typedinray1 = do
    growdiceroll <- lift getCoord -- sample another scatteringpoint?
    if growdiceroll < growprobability
      then do -- yes, sample a new scatteringpoint
        sampledscatterinray <- sampleWithImportanceFrom (RaycastingPointSampler (scene,typedinray1,Sca))
        let scatterinray           = (sampledValue sampledscatterinray)::TypedRay
            scatterinrayimportance = sampledImportance sampledscatterinray
        let nextgrowprobability = 0.6
        sampledfutureinrays <- samplePointRecursively scene nextgrowprobability scatterinray
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
