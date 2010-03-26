{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.MonteCarlo.UtilitySamplers where
  import PIRaTE.MonteCarlo.Sampled
  import Control.Monad.Maybe
  import Control.Monad.State

  randomWeightedChoice :: [(a,Double)] -> UCStreamTo (Sampled a)
  randomWeightedChoice weightedchoices = do
    let weights = snd $ unzip weightedchoices
        totalweight = sum weights
        step                   []  _ = error "cannot choose element from an empty list"
        step ((choice,weight):[])  _ = choice `withProbability` (weight/totalweight)
        step ((choice,weight):wcs) remainingweight
          | remainingweight > weight = step wcs (remainingweight-weight)
          | otherwise = choice `withProbability` (weight/totalweight)
    u1 <- getCoord
    let randomweight = totalweight*(u1::Double)
    return $ (step weightedchoices randomweight)

  -- generates a uniformly distributed random Int in the interval (a,b)
  randomIntInRange :: (Int,Int) -> UCStreamTo Int
  randomIntInRange (a,b) =
    if a==b
      then return a
      else do
        u1 <- getCoord
        let rnddbl = u1::Double
            ad = fromIntegral a
            bd = fromIntegral b
        return $! a + truncate ((bd-ad+1)*rnddbl)

  randomListIndex :: [a] -> UCStreamTo Int
  randomListIndex [] = error "cannot choose an elementindex in an empty list"
  randomListIndex l = randomIntInRange (0,length l - 1)

  randomChoice :: [a] -> UCStreamTo a
  randomChoice choices = do
    rndindex <- randomListIndex choices
    return $ choices!!rndindex

  -- chooses uniformly random a list-element
  instance Sampleable [a] a where
    sampleProbabilityOf choices _ = 1 / (fromIntegral $ length choices)
    --unsafe, should be:
    --sampleProbabilityOf choices choice = if choice `elem` choices then 1 / (fromIntegral $ length choices) else 0
    --requires (Eq a) =>
    sampleWithImportanceFrom choices = do
      choice <- lift . randomChoice $ choices
      return $ choice `withImportance` (fromIntegral $ length choices)
