module PIRaTE.MonteCarlo.UtilitySamplers where
  import PIRaTE.MonteCarlo.Sampled

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