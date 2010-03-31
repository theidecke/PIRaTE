module PIRaTE.Scene.Material (
    Material,
    toHomogenousInteractingMaterial,
    toCustomInteractingMaterial,
    toHomogenousEmittingMaterial,
    toHomogenousSensingMaterial,
    materialAbsorption,
    materialScattering,
    materialExtinction,
    materialScatteringPhaseFunction,
    materialEmissivity,
    materialEmissionDirectedness,
    materialSensitivity,
    materialSensor,
    isInteracting,
    isScattering,
    isAbsorbing,
    isEmitting,
    isSensing
  ) where
  import Data.Monoid
  import qualified Data.WeighedSet as WS
  import PIRaTE.SpatialTypes
  import PIRaTE.Scene.Texture
  import PIRaTE.Scene.PhaseFunction
  import PIRaTE.Scene.Sensor
  
  -- Material contains local absorption and scattering properties
  data Material = Material {
        materialAbsorption              :: Texture Double,
        materialScattering              :: Texture Double,
        materialScatteringPhaseFunction :: PhaseFunction,
        materialEmissivity              :: Texture Double,
        materialEmissionDirectedness    :: PhaseFunction,
        materialSensitivity             :: Texture Double,
        materialSensor                  :: Sensor
    }

  materialExtinction :: Material -> Texture Double
  materialExtinction m = (materialAbsorption m) `mappend` (materialScattering m)

  isInteracting :: Material -> Bool
  isInteracting m = not $ (materialExtinction m)==mempty
  
  isScattering :: Material -> Bool
  isScattering m = not $ (materialScattering m)==mempty
  
  isAbsorbing :: Material -> Bool
  isAbsorbing m = not $ (materialAbsorption m)==mempty
  
  isEmitting :: Material -> Bool
  isEmitting m = not $ (materialEmissivity m)==mempty
  
  isSensing :: Material -> Bool
  isSensing m = not $ (materialSensitivity m)==mempty

  toHomogenousInteractingMaterial :: Double -> Double -> PhaseFunction -> Material
  toHomogenousInteractingMaterial kappa sigma pf =
    Material kappatex sigmatex pf mempty (error "error: undefined16") mempty (error "error: undefined17")
    where kappatex = Homogenous kappa
          sigmatex = Homogenous sigma

  toCustomInteractingMaterial :: (Texture Double) -> (Texture Double) -> PhaseFunction -> Material
  toCustomInteractingMaterial kappatex sigmatex pf =
    Material kappatex sigmatex pf mempty (error "error: undefined19") mempty (error "error: undefined20")

  toHomogenousEmittingMaterial :: Double -> PhaseFunction -> Material
  toHomogenousEmittingMaterial epsilon pf =
    Material mempty mempty (error "error: undefined21") epsilontex pftex mempty (error "error: undefined22")
    where epsilontex = Homogenous epsilon
          pftex | epsilon==0 = (error "error: undefined23")
                | otherwise  = pf
          
  toHomogenousSensingMaterial :: Double -> (PhaseFunction,SensorLogger) -> Material
  toHomogenousSensingMaterial zeta sensor@(pf,sl) =
    Material mempty mempty (error "error: undefined24") mempty (error "error: undefined25") zetatex wsens
    where zetatex = Homogenous zeta
          wsens | zeta==0   = (error "error: undefined26")
                | otherwise = Sensor sensor

  instance Show Material where
    show m =  "kappa="     ++ show (materialAbsorption m) ++
              ", sigma="   ++ show sigma ++
              ", phi_sca=" ++ pfscastring ++
              ", epsilon=" ++ show epsilon ++
              ", phi_emi=" ++ pfemistring ++
              ", zeta="    ++ show zeta ++
              ", phi_sen=" ++ pfsenstring where
                sigma = (materialScattering m)
                epsilon = (materialEmissivity m)
                zeta = (materialSensitivity m)
                pfscastring | isScattering m = show (materialScatteringPhaseFunction m)
                            | otherwise      = "not defined"
                pfemistring | isEmitting m   = show (materialEmissionDirectedness m)
                            | otherwise      = "not defined"
                pfsenstring | isSensing m    = show (materialSensor m)
                            | otherwise      = "not defined"

  instance Monoid Double where
    mempty = 0
    {-# INLINE mempty #-}
    mappend = (+)
    {-# INLINE mappend #-}

