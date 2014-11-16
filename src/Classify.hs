module Classify (classify, trainWMap) where

import qualified Data.Map as M

import Types

maxHamScore, minSpamScore :: Float
maxHamScore = 0.4
minSpamScore = 0.6

classify :: WMap -> [String] -> (MsgType, Float)
classify wm str = let feats = extractFeatures wm str
                      s = score wm feats in
                  (classification s, s)

classification :: Float -> MsgType
classification s | s <= maxHamScore  = Ham
                 | s >= minSpamScore = Spam
                 | otherwise         = Unclear

getWordFeature :: WMap -> String -> WordFeature 
getWordFeature (_, _, m) str = maybe WordFeature 
                                      {word = str, 
                                       hamCount = 0, 
                                       spamCount = 0,
                                       pk = Nothing} id (M.lookup str m)

extractFeatures :: WMap -> [String] -> [WordFeature]
extractFeatures m = map (getWordFeature m) 

trainWMap :: WMap -> [String] -> MsgType -> WMap
trainWMap m s t = foldl (incrementCount t) m s

incrementCount :: MsgType -> WMap -> String -> WMap
incrementCount Ham (hc, sc, m) s = 
    let mfeat = M.lookup s m 
        feat = maybe (getWordFeat s 1 0 Nothing) 
               (\wf -> getWordFeat s (hamCount wf+1) 
                       (spamCount wf) (pk wf)) mfeat 
    in 
      (hc+1, sc, M.insert s feat m)
incrementCount Spam (hc, sc, m) s = 
    let mfeat = M.lookup s m 
        feat = maybe (getWordFeat s 0 1 Nothing) 
               (\wf -> getWordFeat s (hamCount wf) 
                       (spamCount wf + 1) (pk wf)) mfeat 
    in 
      (hc, sc+1, M.insert s feat m)

getWordFeat :: String -> Int -> Int -> Maybe Int -> WordFeature
getWordFeat w ham spam thePK = WordFeature {word = w, 
                                            hamCount = ham, 
                                            spamCount = spam,
                                            pk = thePK} 

spamProb :: WMap -> WordFeature -> Float
spamProb (sc, hc, m) feat = 
    let spamFreq = fromIntegral (spamCount feat) / fromIntegral (max 1 sc)
        hamFreq = fromIntegral (hamCount feat) / fromIntegral (max 1 hc)
    in
      spamFreq / (spamFreq + hamFreq) 

bayesSpamProb (sc, hc, m) feat = 
    let assumedProb =  0.5
        weight = 1
        basicProb = spamProb (sc, hc, m) feat
        dataPoints = fromIntegral (spamCount feat) + fromIntegral (hamCount feat)
    in
      ((weight * assumedProb) + (dataPoints * basicProb)) / (weight + dataPoints)

score :: WMap -> [WordFeature] -> Float
score (sc, hc, m) feats = 
    let spamProbs = map (bayesSpamProb (sc, hc, m)) feats
        hamProbs = map (1.0-) spamProbs
        numProbs = length spamProbs + length hamProbs
        h = 1.0 - fisher spamProbs numProbs
        s = 1.0 - fisher hamProbs numProbs in
    ((1-h) + s) / 2.0

fisher :: [Float] -> Int -> Float
fisher probs numProbs = inverseChiSquare 
                        (sum (map log probs) * negate 2.0) (2*numProbs)

inverseChiSquare :: Float -> Int -> Float
inverseChiSquare value df = 
    if odd df then error "Degree must be even"
    else let m = value / 2.0
             e = exp 1
             sum = e ** negate m
             term = sum
             dfRange = take (df `div` 2) $ iterate (+1) 1
             (sum', term') = foldl (\(s,t) i -> let t' = t * (m/i) in
                                                (s+t', t')) (sum, term) dfRange
         in min sum' 1.0
