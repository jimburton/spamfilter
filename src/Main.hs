{- |
Module      :  Main.hs
Description :  The entry point for the spam program
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

import DBHelper (getWMap, putWMap)
import Classify (classify, trainWMap)
import Types (WMap, MsgType(..))
import Train (train, getWords)

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("train", trainAct)  
            , ("classify", classifyAct)  
            ]  

trainAct :: [String] -> IO ()
trainAct args = do
  let t = (read $ head args) :: MsgType 
      path = head (tail args) 
  getWMap >>= \wm -> train wm path t >>= putWMap . fromJust
  
classifyAct :: [String] -> IO ()
classifyAct (msgPath:args) = do
  let msg = head args
  wm <- getWMap
  ws <- getWords msgPath
  let (typ, score) = classify wm ws
  putStrLn $ show typ ++ ": " ++ show score

main = do  
  args <- getArgs
  if null args 
  then exitWith (ExitFailure 1)
  else do let cmd = head args
              mAct = lookup cmd dispatch  
          case mAct of
            (Just action) -> action (tail args)
            Nothing -> putStrLn "Unknown argument"
