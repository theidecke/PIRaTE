module PerturbationStreams where

  import Data.Word (Word64)
  import Data.List (unfoldr)
  import System.Random.Mersenne.Pure64
  import Control.Monad.State
  
  randomStream :: (PureMT -> (a, PureMT)) -> PureMT -> [a]
  randomStream rndstep g = unfoldr (Just . rndstep) g

  -- | split the generator into an infinite list of generators
  splitInfinitely :: PureMT -> [PureMT]
  splitInfinitely = map pureMT . randomStream randomWord64
  
  randomUCStream :: PureMT -> UCStream
  randomUCStream = randomStream randomDouble

  -- | UnitCoordinatesStream
  type UCStream = [Double]

  accumulatedPerturbedUCStreams :: PureMT ->[UCStream]
  accumulatedPerturbedUCStreams g = scanl step r0 rs where
    step = perturbUCStream defaultPerturbation
    (r0:rs) = map randomUCStream (splitInfinitely g)

  data UCStreamTree = UCStreamTree {
    currentUCStream :: UCStream,
    perturbedUCStreams :: [UCStreamTree]
  }
  
  forwardToPertHead stree = head (perturbedUCStreams stree)
  dropPertHead (UCStreamTree cs (p:ps)) = UCStreamTree cs ps

  unitCoordinatesTreeFromGen :: PureMT -> UCStreamTree
  unitCoordinatesTreeFromGen g = constructTree s0 g1 where
    s0 = randomUCStream g2
    (g1:g2:_) = splitInfinitely g

  constructTree :: UCStream -> PureMT -> UCStreamTree
  constructTree s0 g = UCStreamTree s0 trees where
    trees = zipWith constructTree pss gs1
    pss = map (perturbWith . randomUCStream) gs2
    gs2 = splitInfinitely g'
    (g':gs1) = splitInfinitely g
    perturbWith = perturbUCStream defaultPerturbation s0

  type UCStreamTo = State UCStream
  type Perturbation = Double -> UCStreamTo Double

  perturbUCStream :: Perturbation -> UCStream -> UCStream -> UCStream
  perturbUCStream p cs rs = evalState (mapM p cs) rs

  defaultPerturbation = expPerturbation 0.001 0.01

  expPerturbation :: Double -> Double -> Double -> UCStreamTo Double
  expPerturbation s1 s2 x = do
    r1 <- getCoord
    let dx = s2*(exp (-(log (s2/s1))*r1))
    r2 <- getCoord
    let x' | r2<0.5    = x+dx
           | otherwise = x-dx
        x'' = wrap x'
    return x''

  wrap x | x<0 = wrap (x+1)
         | x>1 = wrap (x-1)
         | otherwise = x

  getCoord :: UCStreamTo Double
  getCoord = do
    coords <- get
    let (c:coords') = coords
    put coords'
    return c
  
  toStream :: Word64 -> UCStream
  toStream seed = randomUCStream $ pureMT seed