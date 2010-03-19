{-# LANGUAGE ParallelListComp #-}

module UCStream (
    UCStream,
    randomUCStream,
    mapSplit,
    PerturbationTree(..),
    forwardToPertHead,
    dropPertHead,
    perturbationTreeFromGen,
    UCStreamTo,
    getCoord
  ) where

  import Data.Word (Word64)
  import Data.List (unfoldr)
  import System.Random.Mersenne.Pure64
  import Control.Monad.State
  
  randomStream :: (PureMT -> (a, PureMT)) -> PureMT -> [a]
  randomStream rndstep g = unfoldr (Just . rndstep) g

  -- | split the generator into an infinite list of generators
  splitInfinitely :: PureMT -> [PureMT]
  splitInfinitely = map pureMT . randomStream randomWord64
  
  mapSplit f g = map f (splitInfinitely g)
  
  randomUCStream :: PureMT -> UCStream
  randomUCStream = randomStream randomDouble

  -- | UnitCoordinatesStream
  type UCStream = [Double]

  accumulatedPerturbedUCStreams :: PureMT ->[UCStream]
  accumulatedPerturbedUCStreams g = scanl step r0 rs where
    step = perturbUCStream defaultPerturbation
    (r0:rs) = mapSplit randomUCStream g

  data PerturbationTree = PerturbationTree {
    currentUCStreams :: [UCStream],
    variations :: [PerturbationTree]
  }
  
  forwardToPertHead stree = head (variations stree)
  dropPertHead (PerturbationTree cs (p:ps)) = PerturbationTree cs ps

  perturbationTreeFromGen :: PureMT -> PerturbationTree
  perturbationTreeFromGen g = constructTree s0 g1 where
    s0 = mapSplit randomUCStream g2
    (g1:g2:_) = splitInfinitely g

  constructTree :: [UCStream] -> PureMT -> PerturbationTree
  constructTree s0 g = PerturbationTree s0 trees where
    trees = zipWith constructTree pss gs1
    pss = [[s `perturbedWith` r | s<-s0 | r<-rs] | rs<-rss]
    rss = mapSplit (mapSplit randomUCStream) g'
    (g':gs1) = splitInfinitely g
    perturbedWith = perturbUCStream defaultPerturbation

  type UCStreamTo = State UCStream
  type Perturbation = Double -> UCStreamTo Double

  perturbUCStream :: Perturbation -> UCStream -> UCStream -> UCStream
  perturbUCStream p cs rs = evalState (mapM p cs) rs

  defaultPerturbation = perturbationFactory (expPerturbation 0.001 0.05)
  --defaultPerturbation = perturbationFactory (perturbation2 2.0 0.05)

  expPerturbation :: Double -> Double -> Double -> Double
  expPerturbation mindx maxdx = let
      c1 = negate $ log (maxdx/mindx)
      c2 = log maxdx
    in \u1 -> exp (c1*u1+c2)

  perturbation2 :: Double -> Double -> Double -> Double
  perturbation2 a maxdx u1 = dx where
    dx = maxdx * utothea / (maxdx*(utothea - 1) + 1)
    utothea = u1**a

  perturbationFactory :: (Double -> Double) -> Double -> UCStreamTo Double
  perturbationFactory u2dx x = do
    r1 <- getCoord
    let dx = u2dx r1
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