{- |
Module      :  Main.hs
Description :  The entry point for the spamfilter program
Copyright   :  (c) Jim Burton
License     :  MIT

Maintainer  :  jimburton1@gmail.com
Stability   :  provisional
Portability :  portable

This is the entry point for the spamfilter program. Its main responsibiilty is to
read the command-line arguments and call the appropriate functions in other
modules.
-}
module Main (main) where

import Data.Char           (toLower, toUpper)
import Data.Maybe          (fromJust)
import System.Environment  (getArgs)
import SpamFilter.Classify (classify)
import SpamFilter.DBHelper (getWMap, putWMap)
import SpamFilter.Train    (getWords, train)
import SpamFilter.Types    (MsgType (..))

-- | Lookup table mapping command-line options to functions.
dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("train", trainAct)
            , ("classify", classifyAct)
            ]

-- | Start training.
trainAct :: [String] -> IO ()
trainAct args = do
  let t = (read $ initCapital $ head args) :: MsgType
      path = head (tail args)
  getWMap >>= train path t >>= putWMap . fromJust

-- | Start classifying.
classifyAct :: [String] -> IO ()
classifyAct (msgPath:_) = do
  wm <- getWMap
  ws <- getWords msgPath
  let (typ, score) = classify wm ws
  putStrLn $ show typ ++ ": " ++ show score
classifyAct _ = usageAndExit

-- | Convert a string to begin with a capital letter, followed by lower case.
initCapital :: String -> String
initCapital ""     = ""
initCapital (x:xs) = toUpper x : map toLower xs

-- | Print the usage message and exit.
usageAndExit :: IO ()
usageAndExit = putStrLn "spamfilter train [Ham|Spam] path/to/email \
\ \nor \nspamfilter classify path/to/email"

-- | The entry point for the program.
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
