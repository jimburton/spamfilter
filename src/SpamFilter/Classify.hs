{- |
Module      :  Classify.hs
Description :  Classifying new messages as ham or spam
Copyright   :  (c) Jim Burton
License     :  MIT

Maintainer  :  j.burton@brighton.ac.uk
Stability   :  provisional
Portability :  portable

Module which provides functions to classify an individual message as
Ham|Spam|Unclear. (All of the functions in this module are pure and may be
a good target for parallelisation...)
-}
module SpamFilter.Classify
    (classify, getWordFeature, getWordFeat)
    where

import qualified Data.Map         as M
import SpamFilter.Types ( WordFeature(..), MsgType(..), WMap )
import Data.Maybe (fromMaybe)

{-| A message with a higher rating than maxHamScore is not ham. -}
maxHamScore :: Float
maxHamScore = 0.46

{-| A message with a lower rating than minSpamScore is not spam. -}
minSpamScore :: Float
minSpamScore = 0.56

{-| Classify the contents of a message as Ham|Spam|Unclear, based on
the contents of the WMap. -}
classify :: WMap -> [String] -> (MsgType, Float)
classify wm str = let feats = extractFeatures wm str
                      s = score wm feats in
                  (classification s, s)

{-| Returns the type associated with a given score. -}
classification :: Float -> MsgType
classification s | s <= maxHamScore  = Ham
                 | s >= minSpamScore = Spam
                 | otherwise         = Unclear

{-| Turn a list of strings into a list of WordFeatures. -}
extractFeatures :: WMap -> [String] -> [WordFeature]
extractFeatures m = map (getWordFeature m)

{-| Look up a word in the WMap, retrieving the WordFeature associated
with this word. If it isn't in the WMap yet, create a new WordFeature. -}
getWordFeature :: WMap -> String -> WordFeature
getWordFeature (_, _, m) str = fromMaybe WordFeature
                                      {word = str,
                                       hamCount = 0,
                                       spamCount = 0,
                                       pk = Nothing} (M.lookup str m)

{-| Wrapper for the WordFeature type constructor. -}
getWordFeat :: String -> Int -> Int -> Maybe Int -> WordFeature
getWordFeat w ham spam thePK = WordFeature { word = w
                                           , hamCount = ham
                                           , spamCount = spam
                                           , pk = thePK }

{-| The basic probability that a WordFeature contains a spam word. -}
spamProb :: WMap -> WordFeature -> Float
spamProb (sc, hc, _) feat =
    let spamFreq = fromIntegral (spamCount feat) / fromIntegral (max 1 sc)
        hamFreq = fromIntegral (hamCount feat) / fromIntegral (max 1 hc)
    in
      spamFreq / (spamFreq + hamFreq)

{-| The Bayesean probability that a WordFeature contains a spam word --
i.e., the probability based on the probilities of the other words in the message
being spam. -}
bayesSpamProb :: (Int, Int, M.Map String WordFeature)
                       -> WordFeature -> Float
bayesSpamProb (sc, hc, m) feat =
    let assumedProb =  0.5
        weight = 1
        basicProb = spamProb (sc, hc, m) feat
        dataPoints = fromIntegral (spamCount feat) + fromIntegral (hamCount feat)
    in
      ((weight * assumedProb) + (dataPoints * basicProb)) / (weight + dataPoints)

{-| Produce a score for a list of WordFeatures representing an individual message. -}
score :: WMap -> [WordFeature] -> Float
score (sc, hc, m) feats =
    let spamProbs = map (bayesSpamProb (sc, hc, m)) feats
        hamProbs = map (1.0-) spamProbs
        numProbs = length spamProbs + length hamProbs
        h = 1.0 - fisher spamProbs numProbs
        s = 1.0 - fisher hamProbs numProbs in
    ((1-h) + s) / 2.0

{-| Fisher's combined probability test. -}
fisher :: [Float] -> Int -> Float
fisher probs numProbs = inverseChiSquare
                        (sum (map log probs) * negate 2.0) (2*numProbs)

{-| The inverse chi-squared function. -}
inverseChiSquare :: Float -> Int -> Float
inverseChiSquare value df =
    if odd df then error "Degree must be even"
    else let m = value / 2.0
             e = exp 1
             theSum = e ** negate m
             term = theSum
             dfRange = take (df `div` 2) $ iterate (+1) 1
             (sum', _) = foldl (\(s,t) i -> let t' = t * (m/i) in
                                            (s+t', t')) (theSum, term) dfRange
         in min sum' 1.0
