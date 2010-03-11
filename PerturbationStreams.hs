module PerturbationStreams where

  import Data.Word (Word64)
  import Data.List (unfoldr)
  import System.Random.Mersenne.Pure64
  import Control.Monad.State
  
  randomStream :: (PureMT -> (a, PureMT)) -> PureMT -> [a]
  randomStream rndstep g = unfoldr (Just . rndstep) g
  
  type Stream = [Double]
  type StreamState a = State Stream a

  getCoord :: StreamState Double
  getCoord = do
    gen <- get
    let (coord:gen') = gen
    put gen'
    return coord

  randomDoubleStream :: PureMT -> Stream
  randomDoubleStream = randomStream randomDouble
  
  -- | split the generator into an infinite list of generators
  splitInfinitely :: PureMT -> [PureMT]
  splitInfinitely = map pureMT . randomStream randomWord64

  accumulatedPerturbedStreams :: PureMT ->[Stream]
  accumulatedPerturbedStreams g = scanl step r0 rs where
    step = perturbStream defaultPerturbation
    (r0:rs) = map randomDoubleStream (splitInfinitely g)

  data StreamTree = StreamTree {
    currentStream :: Stream,
    perturbedStreams :: [StreamTree]
  }
  
  forwardToPertHead stree = head (perturbedStreams stree)
  dropPertHead (StreamTree cs (p:ps)) = StreamTree cs ps

  streamTreeFromGen :: PureMT -> StreamTree
  streamTreeFromGen g = constructTree s0 g1 where
    s0 = randomDoubleStream g2
    (g1:g2:_) = splitInfinitely g

  constructTree :: Stream -> PureMT -> StreamTree
  constructTree s0 g = StreamTree s0 trees where
    trees = zipWith constructTree pss gs1
    pss = map (perturbWith . randomDoubleStream) gs2
    gs2 = splitInfinitely g'
    (g':gs1) = splitInfinitely g
    perturbWith = perturbStream defaultPerturbation s0

  type Perturbation = (Double,Stream) -> (Double,Stream)

  perturbStream :: Perturbation -> Stream -> Stream -> Stream
  perturbStream p (c:cs) rs = c' : perturbStream p cs rs' where
    (c',rs') = p (c,rs)

  defaultPerturbation :: Perturbation
  defaultPerturbation = expPerturbation 0.001 0.01
  
  expPerturbation s1 s2 (x,rs) = (x',rs') where
    x' = wrap x''
    x'' | r1<0.5    = x+dx
        | otherwise = x-dx
    dx = s2*(exp (-(log (s2/s1))*r2))
    (r1:r2:rs') = rs
    wrap x | x<0 = wrap (x+1)
           | x>1 = wrap (x-1)
           | otherwise = x

  teststream :: Stream
  teststream = randomDoubleStream $ pureMT 17