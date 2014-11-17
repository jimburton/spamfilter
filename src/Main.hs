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

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Data.Maybe (fromJust)
import Control.Monad (foldM, mapM_)

import DBHelper (getWMap, putWMap)
import Classify (classify)
import Types (WMap, MsgType(..))
import Train (train, trainWMap, getWords)

{-| Lookup table mapping command-line options to functions. -}
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("train", trainAct)  
            , ("classify", classifyAct)  
            ]  

{-| Start training. -}
trainAct :: [String] -> IO ()
trainAct args = do
  let t = (read $ head args) :: MsgType 
      path = head (tail args) 
  getWMap >>= \wm -> train wm path t >>= putWMap . fromJust
  
{-| Start classifying. -}
classifyAct :: [String] -> IO ()
classifyAct (msgPath:args) = do
  let msg = head args
  wm <- getWMap
  ws <- getWords msgPath
  let (typ, score) = classify wm ws
  putStrLn $ show typ ++ ": " ++ show score

{-| The entry point for the program. -}
main = do  
  args <- getArgs
  if null args 
  then exitWith (ExitFailure 1)
  else do let cmd = head args
              mAct = lookup cmd dispatch  
          case mAct of
            (Just action) -> action (tail args)
            Nothing -> putStrLn "Unknown argument"

{-| This should be in a test module-}
trainAndTest = do 
  wm <- getWMap
  let hams = ["/home/jb259/sa-corpus/easy_ham"
             , "/home/jb259/sa-corpus/easy_ham"
             , "/home/jb259/sa-corpus/hard_ham"]
      spams = ["/home/jb259/sa-corpus/spam"
              , "/home/jb259/sa-corpus/spam_2"]
      pathToMail = "/home/jb259/haskell/src/spamfilter/etc/mail/"
      testMsgs = ["ham1.email", "ham2.email", "ham3.email", "ham4.email"
                 , "ham5.email", "spam1.email", "spam2.email", "spam3.email"
                 , "spam4.email", "spam5.email"]
  Just wm' <- foldM (\wm path -> train (fromJust wm) path Ham) (Just wm) hams
  Just wm'' <- foldM (\wm path -> train (fromJust wm) path Spam) (Just wm') spams
  mapM_ (\msg -> do ws <- getWords (pathToMail ++ msg)
                    let (typ, score) = classify wm'' ws
                    putStrLn $ msg ++ ": " ++ show typ ++ ": " ++ show score) testMsgs
