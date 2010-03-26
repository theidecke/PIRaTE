--{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.Scene.Sensor where
  import PIRaTE.SpatialTypes
  import qualified Data.WeighedSet as WS
  import PIRaTE.Scene.PhaseFunction
  import PIRaTE.MonteCarlo.Sampled
  import PIRaTE.MonteCarlo.UtilitySamplers (randomWeightedChoice)
  
  -- the kinds of sensor data we might want to log  
  data SensorResult = Grid2DPhoton (Int,Int) Double
                    | PathLength Int
  
  type SensorLogger = MLTState -> SensorResult
  newtype Sensor = Sensor {sensorPFwithLogger :: (PhaseFunction,SensorLogger)}
  instance Show Sensor where
    show _ = "Sensor"

  sensorLogger :: Sensor -> SensorLogger
  sensorLogger (Sensor (_,sl)) = sl
  sensorDirectedness :: Sensor -> PhaseFunction
  sensorDirectedness (Sensor (pf,_)) = pf

  type WeightedSensor = [(Sensor,Double)]

  instance Sampleable (WeightedSensor,Point) Direction where
    sampleProbabilityOf (wpflist,origin) wout
      | totalweight==0 = 0
      | otherwise = (/totalweight) . sum $ [w*(sampleProbabilityOf (sensorDirectedness sensor,inray) wout) | (sensor,w) <- wpflist]
      where totalweight = sum . snd . unzip $ wpflist
            inray = Ray origin undefined

    sampleFrom (wpf,origin) = do
      sampledphasefunction <- lift . randomWeightedChoice $ wpf
      let pf = sensorDirectedness . sampledValue $ sampledphasefunction
          inray = Ray origin undefined
      sampleFrom (pf,inray)
