{- |
Module      :  Main.hs
Description :  The entry point for the spamfilter program
Copyright   :  (c) Jim Burton
License     :  MIT

Maintainer  :  j.burton@brighton.ac.uk
Stability   :  provisional
Portability :  portable

This is the entry point for the spamfilter program. Its main responsibiilty is to
read the command-line arguments and call the appropriate functions in other
modules.
-}
module Main where

import           Control.Applicative ((<$>))
import           Data.Char           (toLower, toUpper)
import           Data.Maybe          (fromJust)
import           System.Environment  (getArgs)
import           System.FilePath     ((</>))
import           SpamFilter.Classify (classify)
import           SpamFilter.DBHelper (getWMap, putWMap)
import           SpamFilter.Train    (getWords, train)
import           SpamFilter.Types    (MsgType (..), WMap)

{-| Lookup table mapping command-line options to functions. -}
dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("train", trainAct)
            , ("classify", classifyAct)
            ]

{-| Start training. -}
trainAct :: [String] -> IO ()
trainAct args = do
  let t = (read $ toCamelCase $ head args) :: MsgType
      path = head (tail args)
  getWMap >>= \wm -> train wm path t >>= putWMap . fromJust

{-| Start classifying. -}
classifyAct :: [String] -> IO ()
classifyAct (msgPath:_) = do
  wm <- getWMap
  ws <- getWords msgPath
  let (typ, score) = classify wm ws
  putStrLn $ show typ ++ ": " ++ show score
classifyAct _ = usageAndExit

toCamelCase :: String -> String
toCamelCase ""     = ""
toCamelCase (x:xs) = toUpper x : (map toLower xs)

{-| This should be in a test module-}
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

usageAndExit :: IO ()
usageAndExit = putStrLn "spamfilter train path/to/email [Ham|Spam] \
\ \nor \nspamfilter classify path/to/email"

{-| The entry point for the program. -}
main :: IO ()
main = do
  args <- getArgs
  if null args
  then usageAndExit
  else do let cmd = head args
              mAct = lookup cmd dispatch
          case mAct of
            (Just action) -> action (tail args)
            Nothing       -> do putStrLn "Unknown argument"
                                usageAndExit
