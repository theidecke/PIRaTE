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
    Material kappatex sigmatex pf mempty undefined mempty undefined

  toHomogenousEmittingMaterial :: Double -> PhaseFunction -> Material
  toHomogenousEmittingMaterial epsilon pf =
    Material mempty mempty undefined epsilontex pftex mempty undefined
    where epsilontex = Homogenous epsilon
          pftex | epsilon==0 = undefined
                | otherwise  = pf
          
  toHomogenousSensingMaterial :: Double -> (PhaseFunction,SensorLogger) -> Material
  toHomogenousSensingMaterial zeta sensor@(pf,sl) =
    Material mempty mempty undefined mempty undefined zetatex wsens
    where zetatex = Homogenous zeta
          wsens | zeta==0   = undefined
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
                            | otherwise      = "undefined"
                pfemistring | isEmitting m   = show (materialEmissionDirectedness m)
                            | otherwise      = "undefined"
                pfsenstring | isSensing m    = show (materialSensor m)
                            | otherwise      = "undefined"

  instance Monoid Double where
    mempty = 0
    {-# INLINE mempty #-}
    mappend = (+)
    {-# INLINE mappend #-}

