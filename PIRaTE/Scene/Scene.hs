{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module PIRaTE.Scene.Scene (
    Entity,
    entityContainer,
    containing,
    entityFromContainerAndMaterials,
    absorptionAt,
    scatteringAt,
    extinctionAt,
    albedoAt,
    sensitivityAt,
    emissivityAt,
    scatterPhaseFunctionsAt,
    emissionDirectednessesAt,
    sensorDirectednessesAt,
    Scene,
    sceneFromEntities,
    sceneEntities,
    sceneEmitters,
    sceneInteractors,
    sceneScatterers,
    sceneAbsorbers,
    sceneSensors,
    opticalDepthBetween,
    probeExtinction,
    probeExtinctionClosure,
    probeScattering,
    probeScatteringClosure,
    probeEmission,
    probeEmissivityClosure,
    probeSensitivity,
    probeSensitivityClosure,
    ProbeResult,
    getProbeResultDepth,
    getProbeResultDist
  ) where
  import Data.ACVector ((|*),vmag,Vector3(..))
  import Data.Monoid
  --import Data.Maybe (fromMaybe,fromJust,isNothing,isJust)
  import Data.Maybe (fromJust)
  --import Data.Array.Vector (singletonU)
  import qualified Data.List as L
  import qualified Data.Set as S
  import Data.WeighedSet (fromWeightList)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (infinity,edgeMap)
  import PIRaTE.Scene.Confineable
  import PIRaTE.Scene.Container
  import PIRaTE.Scene.Container.Sphere
  import PIRaTE.Scene.PhaseFunction
  import PIRaTE.Scene.PhaseFunction.Isotropic
  import PIRaTE.Scene.PhaseFunction.ZCone
  import PIRaTE.Scene.Texture
  import PIRaTE.Scene.Material
  import PIRaTE.Scene.Sensor
  
  -- an Entity is a container filled with light-influencing material
  data Entity = Entity {
      entityContainer::Container,
      entityMaterials::[Material]
    }
  
  entityFromContainerAndMaterials :: Container -> [Material] -> Entity
  entityFromContainerAndMaterials container materials = Entity container materials
  
  instance Show Entity where
    show (Entity container materials) = "Entity contained by a " ++ (show container) ++
                                        " filled with " ++ (show materials)
  
  {--randomPointInEntities :: [Entity] -> Gen s -> ST s Point
  randomPointInEntities entities g = do
    entity <- randomSampleFrom entities g
    let container = entityContainer entity
    randomSampleFrom container g--}
  
  isEmitter :: Entity -> Bool
  isEmitter (Entity _ materials) = any isEmitting materials
  {-# INLINE isEmitter #-}
  
  isInteractor :: Entity -> Bool
  isInteractor (Entity _ materials) = any isInteracting materials
  {-# INLINE isInteractor #-}
  
  isScatterer :: Entity -> Bool
  isScatterer (Entity _ materials) = any isScattering materials
  {-# INLINE isScatterer #-}
  
  isAbsorber :: Entity -> Bool
  isAbsorber (Entity _ materials) = any isAbsorbing materials
  {-# INLINE isAbsorber #-}
  
  isSensor :: Entity -> Bool
  isSensor (Entity _ materials) = any isSensing materials
  {-# INLINE isSensor #-}
  
  containing :: [Entity] -> Point -> [Entity]
  containing entities point = filter ((`contains` point).entityContainer) entities
  
  materialsAt :: [Entity] -> Point -> [Material]
  materialsAt entities point = concatMap entityMaterials $ entities `containing` point
  {-# INLINE materialsAt #-}

  propertyAt :: (Material->Texture Double) -> [Entity] -> Point -> Double
  propertyAt getpropfrom entities point = sum [getpropfrom m `evaluatedAt` point | m<-mats]
    where mats = entities `materialsAt` point
  {-# INLINE propertyAt #-}
    
  absorptionAt :: Scene -> Point -> Double
  absorptionAt scene = propertyAt materialAbsorption (sceneAbsorbers scene)
  {-# INLINE absorptionAt #-}
    
  scatteringAt :: Scene -> Point -> Double
  scatteringAt scene = propertyAt materialScattering (sceneScatterers scene)
  {-# INLINE scatteringAt #-}

  extinctionAt :: Scene -> Point -> Double
  extinctionAt scene = propertyAt materialExtinction (sceneInteractors scene)
  {-# INLINE extinctionAt #-}

  albedoAt :: Scene -> Point -> Double
  albedoAt scene point
    | sigma==0 = 0
    | otherwise = sigma/xi
    where sigma = scatteringAt scene point
          xi    = extinctionAt scene point
  {-# INLINE albedoAt #-}

  emissivityAt :: Scene -> Point -> Double
  emissivityAt scene = propertyAt materialEmissivity (sceneEmitters scene)
  {-# INLINE emissivityAt #-}

  sensitivityAt :: Scene -> Point -> Double
  sensitivityAt scene = propertyAt materialSensitivity (sceneSensors scene)
  {-# INLINE sensitivityAt #-}
  
  scatterPhaseFunctionsAt :: Scene -> Point -> WeightedPhaseFunction
  scatterPhaseFunctionsAt scene point =
    [m `phaseFunctionWithWeightAt` point | m<-mats]
    where phaseFunctionWithWeightAt m p = (materialScatteringPhaseFunction m, materialScattering m `evaluatedAt` p)
          mats = scatterers `materialsAt` point
          scatterers = sceneScatterers scene

  emissionDirectednessesAt :: Scene -> Point -> WeightedPhaseFunction
  emissionDirectednessesAt scene point =
    [m `phaseFunctionWithWeightAt` point | m<-mats]
    where phaseFunctionWithWeightAt m p = (materialEmissionDirectedness m, materialEmissivity m `evaluatedAt` p)
          mats = emitters `materialsAt` point
          emitters = sceneEmitters scene

  sensorDirectednessesAt :: Scene -> Point -> WeightedSensor
  sensorDirectednessesAt scene point =
    [m `phaseFunctionWithWeightAt` point | m<-mats]
    where phaseFunctionWithWeightAt m p = (materialSensor m, materialSensitivity m `evaluatedAt` p)
          mats = sensors `materialsAt` point
          sensors = sceneSensors scene

  -- a Scene contains all entities
  sceneFromEntities entities = Scene {
    sceneEntities    = entities,
    sceneEmitters    = filter isEmitter entities,
    sceneInteractors = filter isInteractor entities,
    sceneScatterers  = filter isScatterer entities,
    sceneAbsorbers   = filter isAbsorber entities,
    sceneSensors     = filter isSensor entities
  }
  
  data Scene = Scene {
      sceneEntities::[Entity],
      sceneEmitters::[Entity],
      sceneInteractors::[Entity],
      sceneScatterers::[Entity],
      sceneAbsorbers::[Entity],
      sceneSensors::[Entity]
    }

  instance Show Scene where
    show scene = show (sceneEntities scene)

  -- the Bool is used to represent if the IntervalLimiter is the begin of an interval
  data IntervalLimiter a = IntervalLimiter {
      intervalLimiterKey::a,
      isIntervalLimiterBegin::Bool,
      intervalLimiterPosition::Double
    }
  instance (Show a) => Show (IntervalLimiter a) where
    show (IntervalLimiter key isbegin pos) =
      (if isbegin then "Begin(" else "End(") ++ (show key) ++ "@" ++ (show pos) ++ ")"
  -- we use the equality just in terms of position (for Ord)
  instance Eq (IntervalLimiter a) where
    (==) (IntervalLimiter _ _ pos1) (IntervalLimiter _ _ pos2) = pos1==pos2
    {-# INLINE (==) #-}
  -- we order IntervalLimiters by position
  instance Ord (IntervalLimiter a) where
    (<=) (IntervalLimiter _ _ pos1) (IntervalLimiter _ _ pos2) = pos1<=pos2
    {-# INLINE (<=) #-}
    
  
  fromKeyInterval :: a -> Interval -> [IntervalLimiter a]
  fromKeyInterval key (start,end) = [IntervalLimiter key  True start,
                                     IntervalLimiter key False   end]
  {-# INLINE fromKeyInterval #-}
                                  
  -- transforms a list of tagged intervals possibly overlapping and with coinciding
  -- endpoints into a list of disjoint intervals with taglist
  cutOverlaps :: (Ord a) => [IntervalLimiter a] -> [(Interval,S.Set a)]
  cutOverlaps limiters = cutOverlaps' S.empty sortedLimiters
    where sortedLimiters = L.sort limiters
          cutOverlaps' :: (Ord a) => (S.Set a) -> [IntervalLimiter a] -> [(Interval, S.Set a)]
          cutOverlaps' active         [] = []
          cutOverlaps' active (l1:l2:ls)
            | S.null newactive || l1==l2 = rest
            | otherwise = ((intervalLimiterPosition l1,
                            intervalLimiterPosition l2), newactive):rest
            where newactive = getNewActive l1
                  rest = cutOverlaps' newactive (l2:ls)
                  getNewActive (IntervalLimiter key isbegin _) =
                    (if isbegin then S.insert else S.delete) key active
          cutOverlaps' active ((IntervalLimiter _ isbegin _):[]) =
            if isbegin then error "last intervallimiter shouldn't be a begin" else []  
  
  {--
  cutoverlapstestcase = concat.map (uncurry fromInterval) $
    zip [(1,5),(1,7),(2,6),(2,5),(3,4),(1,5)] [1,2,3,4,5,6]
  testCutOverlaps = cutOverlaps cutoverlapstestcase == [
      ((1.0,2.0),S.fromList [1,2,6])      ,((2.0,3.0),S.fromList [1,2,3,4,6]),
      ((3.0,4.0),S.fromList [1,2,3,4,5,6]),((4.0,5.0),S.fromList [1,2,3,4,6]),
      ((5.0,6.0),S.fromList [2,3])        ,((6.0,7.0),S.fromList [2])
    ]
    --}


  disjointIntervalsWithMaterials :: [Entity] -> Ray -> [(Interval,[Material])]
  disjointIntervalsWithMaterials entities ray =
    zip disjointintervals intervalmaterials
    where
      disjointintervals = fst $ unzip taggeddisjointintervals
      intervalmaterials = getIntervalMaterials entities taggeddisjointintervals
      taggeddisjointintervals    = getTaggedDisjointIntervals entities ray      

  -- | returns a list of pairs of disjoint intervals with a set of entityindices which are contained in the interval
  getTaggedDisjointIntervals :: [Entity] -> Ray -> [(Interval, S.Set Int)]
  getTaggedDisjointIntervals entities ray =
    cutOverlaps . concat $ concatMap distributeEntityTag nestedindexedintervals
    where
      distributeEntityTag (entityindex, intervals) = map (fromKeyInterval entityindex) intervals
      nestedindexedintervals = zip [(0::Int)..] entityintersections
      entityintersections = [entityContainer entity `intersectedBy` ray | entity<-entities]

  -- | returns in listform for every disjoint interval the list of materials of all entities present in the interval
  getIntervalMaterials :: [Entity] -> [(Interval, S.Set Int)] -> [[Material]]
  getIntervalMaterials entities taggeddisjointintervals =
    [concat [entitymateriallists!!entityindex | entityindex<-entityindexset] | entityindexset <- intervalentityindexsets]
    where
      intervalentityindexsets = map (S.toList . snd) taggeddisjointintervals
      entitymateriallists = map entityMaterials entities

  -- sort out intervals that are before the ray starts or further away than maxDist
  -- and clip intervals that span across these bounds
  clipAndFilterIntervalsWithMaterial :: Double -> [(Interval,[Material])] -> [(Interval,[Material])]
  clipAndFilterIntervalsWithMaterial maxDist intervalswithmaterials = let
      maxDist' = max 0 maxDist
      outsideOfInterest = uncurry (\x y -> x>=maxDist' || y<=0) . fst
      filterIntervals = filter (not.outsideOfInterest)
      clipInterval (x,y) = (max 0 x, min maxDist' y)
      clipIntervals = map (uncurry (\x y -> (clipInterval x, y)))
    in filterIntervals . clipIntervals $ intervalswithmaterials
  
  
  data ProbeResult = MaxDepthAtDistance Double | MaxDistAtDepth Double deriving (Show)
  getProbeResultDepth (MaxDistAtDepth depth) = Just depth
  getProbeResultDepth _ = Nothing
  {-# INLINE getProbeResultDepth #-}
  getProbeResultDist (MaxDepthAtDistance distance) = Just distance
  getProbeResultDist _ = Nothing
  {-# INLINE getProbeResultDist #-}
  
  consumeIntervals :: Ray -> Double -> Double -> [(Interval, Texture Double)] -> ProbeResult
  consumeIntervals ray maxDepth accumulatedDepth [] = MaxDistAtDepth accumulatedDepth
  consumeIntervals ray maxDepth accumulatedDepth (((a,b), sf):rest)
    | isHomogenous sf = case homogenousraymarchresult of
        (MaxDistAtDepth    intervaldepth) -> consumeIntervals ray maxDepth (accumulatedDepth + intervaldepth) rest
        (MaxDepthAtDistance maxdepthdist) -> MaxDepthAtDistance maxdepthdist
    | otherwise     = consumeIntervals ray maxDepth accumulatedDepth (discretizedinhominterval++rest)
    where
      homogenousraymarchresult = rayMarchHomogenousInterval scalarfunction (a,b) remainingDepth
      discretizedinhominterval = discretizeInterval scalarfunction (a,b)
      scalarfunction s = sf `evaluatedAt` (ray `followFor` s)
      remainingDepth = maxDepth - accumulatedDepth
  
  rayMarchHomogenousInterval f (a,b) remainingdepth
    | remainingdepth > intervaldepth = MaxDistAtDepth intervaldepth
    | otherwise                      = MaxDepthAtDistance (a+neededdist)
    where
      neededdist = remainingdepth / scalarvalue
      intervaldepth = scalarvalue * intervallength
      intervallength = b - a
      scalarvalue = f (error "error: undefined15") -- only works for Homogenous Materials

  discretizeInterval :: (Double->Double) -> (Double, Double) -> [(Interval, Texture Double)]
  discretizeInterval f (a,b) = gaussDiscretizer 0.02 f (a,b)

  gaussDiscretizer maxstep f (a,b) = edgeMap toIntMat intervalbounds where
    toIntMat a' b' = ((a',b'), Homogenous avgf)
      where avgf = averager a' b'
    averager = curry (gaussAverager f)
    intervalbounds = [a,a+h0..b]
    h0 = intervallength / (fromIntegral steps) --min maxstep intervallength
    steps = ceiling $ intervallength / maxstep 
    intervallength = b - a

  gaussAverager f (a,b) = meanf where
    meanf = {--(h*) .--} sum $ zipWith (*) gaussweights samples
    samples = map (f . (\t->a+h*t)) gausspositions
    h = b-a
    (gausspositions,gaussweights) = gauss4

  gauss1 = ([0.5],[1])
  gauss2 = ([0.2113248654051871, 0.7886751345948129],[0.5,0.5])
  gauss3 = ([0.1127016653792583,0.5,0.8872983346207417],[5/18,4/9,5/18])
  gauss4 = ([0.06943184420297371, 0.3300094782075719, 0.6699905217924281, 0.930568155797026], 
            [0.1739274225687269, 0.3260725774312731, 0.3260725774312731, 0.1739274225687269])
  gauss5 = ([0.04691007703066800, 0.2307653449471585, 0.5, 0.7692346550528415, 0.953089922969332], 
            [0.1184634425280945, 0.2393143352496832, 0.2844444444444444, 0.2393143352496832, 0.1184634425280945])
  
  -- casts a Ray through a list of entities until either a maximum optical depth
  -- or a maximum distance is reached
  probePropertyOfEntitiesWithRay :: (Material -> Texture Double) -> [Entity] -> Ray -> Distance -> Double -> ProbeResult
  probePropertyOfEntitiesWithRay propertyof entities ray maxDist maxDepth =
    probePropertyOfEntitiesWithRayClosure propertyof entities ray (maxDist,maxDepth)

  matListToTexture :: (Material -> Texture Double) -> (Interval,[Material]) -> (Interval, Texture Double)
  matListToTexture propertyof = (\(i,ms)->(i,mconcat $ map propertyof ms))

  probePropertyOfEntitiesWithRayClosure :: (Material -> Texture Double) -> [Entity] -> Ray -> ((Distance,Double) -> ProbeResult)
  probePropertyOfEntitiesWithRayClosure propertyof entities ray = let
      refinedintervalswithtextures = disjointIntervalsWithMaterials entities ray
      matsToTexture = matListToTexture propertyof
    in \(maxDist,maxDepth) ->
         (consumeIntervals ray maxDepth 0
                           (map matsToTexture (clipAndFilterIntervalsWithMaterial maxDist refinedintervalswithtextures)) )
  
  probeExtinctionClosure scene ray =
    probePropertyOfEntitiesWithRayClosure materialExtinction interactors ray where
      interactors = sceneInteractors scene

  probeScatteringClosure scene ray =
    probePropertyOfEntitiesWithRayClosure materialScattering scatterers ray where
      scatterers = sceneScatterers scene

  probeEmissivityClosure scene ray =
    probePropertyOfEntitiesWithRayClosure materialEmissivity emitters ray where
      emitters = sceneEmitters scene

  probeSensitivityClosure scene ray =
    probePropertyOfEntitiesWithRayClosure materialSensitivity sensors ray where
      sensors = sceneSensors scene

  probeExtinction  scene ray maxdist maxdepth = probeExtinctionClosure  scene ray (maxdist,maxdepth)
  probeScattering  scene ray maxdist maxdepth = probeScatteringClosure  scene ray (maxdist,maxdepth)
  probeEmission    scene ray maxdist maxdepth = probeEmissivityClosure  scene ray (maxdist,maxdepth)
  probeSensitivity scene ray maxdist maxdepth = probeSensitivityClosure scene ray (maxdist,maxdepth)

  depthOfBetween :: (Material -> Texture Double) -> [Entity] -> Point -> Point -> Double
  depthOfBetween propertyof entities v w
    | v==w = 0
    | otherwise = fromJust $ getProbeResultDepth proberesult
    where
      distance = vmag $ w - v
      ray = Ray v (Direction $ (1/distance)|*(w-v))
      proberesult = probePropertyOfEntitiesWithRay propertyof entities ray distance infinity

  opticalDepthBetween :: Scene -> Point -> Point -> Double
  opticalDepthBetween scene = depthOfBetween materialExtinction (sceneInteractors scene)
  {-# INLINE opticalDepthBetween #-}
