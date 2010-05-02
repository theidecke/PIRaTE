module PIRaTE.SpatialTypes where
  import Data.ACVector
  import Text.Printf (printf)
  import PIRaTE.UtilityFunctions (normalize)
  
  -- some basic aliases and helper data types
  type Point = Vector3
  showVector3 v = "{"++ (printf "%f" (v3x v)) ++", "++ (printf "%f" (v3y v)) ++", "++ (printf "%f" (v3z v)) ++"}"

  type Distance = Double
  newtype Direction = Direction Vector3
  unDirection (Direction d) = d
  appliedToDirection f (Direction d) = Direction (f d)
  fromEdge e | e `vdot` e==0 = error "fromEdge: cannot infer direction from edge with zero length"
             | otherwise = Direction (normalize e)
  
  instance Show Direction where
    show (Direction d) = "-->" ++ showVector3 d
  instance Eq Direction where
    (==) (Direction d1) (Direction d2) = (==) d1 d2
  
  arcsec = 2*pi/1296000
  arcmin = 60*arcsec
  degree = 60*arcmin
  
  type Path = [Point]
  pathLength path = length path - 1
  pathNodeCount path = length path
  showQuantizedPath = show . map quantizeNode where
    quantizeNode (Vector3 x y z) = (round (1000*x),round (1000*y),round (1000*z))
  type MLTState = Path
  fromPath :: Path -> MLTState
  fromPath = id
  mltStatePath :: MLTState -> Path
  mltStatePath = id
  mltStatePathLength :: MLTState -> Int
  mltStatePathLength = pathLength . mltStatePath
  mltStateSubstitutePath :: MLTState -> Path -> MLTState
  mltStateSubstitutePath oldstate newpath = newpath

  showMLTState = concatMap showVector3 . mltStatePath

  data Ray = Ray {
      rayOrigin::Point,
      rayDirection::Direction
    } deriving (Eq)
    
  followFor :: Ray -> Double -> Point
  followFor (Ray origin (Direction direction)) distance = origin + distance |* direction
  
  instance Show Ray where
    show (Ray o d) = "Ray starting at "++ (showVector3 o) ++" going in Direction "++ (show d) 

  fromTwoPoints :: Point -> Point -> Ray
  fromTwoPoints v w | v==w      = error "fromTwoPoints: cannot infer direction between two equal points"
                    | otherwise = Ray v (Direction $ normalize (w-v))
  
  type Interval = (Double,Double)
