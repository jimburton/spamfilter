module Main where
{-
TODO -- write some tests!
-}

import Control.Applicative ((<$>))
import System.FilePath     ((</>))

import SpamFilter.Types    (WMap)

trainOnCorpus :: IO WMap
trainOnCorpus = do
  wm <- getWMap
  let hamPath = "/home/jb259/mail-corpora/ham"
      spamPath = "/home/jb259/mail-corpora/spam"
  Just wm' <- train wm hamPath Ham
  fromJust <$> train wm' spamPath Spam

testOnSamples :: WMap -> IO ()
testOnSamples wm = do
  let pathToMail = "/home/jb259/spamfilter/etc/mail/"
      testMsgs = ["ham1.email", "ham2.email", "ham3.email", "ham4.email"
                 , "ham5.email", "spam1.email", "spam2.email", "spam3.email"
                 , "spam4.email", "spam5.email"]
  mapM_ (\msg -> do ws <- getWords (pathToMail </> msg)
                    let (typ, score) = classify wm ws
                    putStrLn $ msg ++ ": " ++ show typ ++ ": " ++ show score) testMsgs


trainAndTest :: IO ()
trainAndTest = trainOnCorpus >>= testOnSamples

trainAndStore :: IO ()
trainAndStore = trainOnCorpus >>= putWMap
