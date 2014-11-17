{- |
Module      :  Train.hs
Description :  Functions for training the spam program
Copyright   :  (c) Jim Burton
License     :  MIT

Maintainer  :  j.burton@brighton.ac.uk
Stability   :  provisional 
Portability :  portable 

Contains functions devoted to reading in mail messages and passing their contents 
to functions in the classify module. 
-}
module Train (train, getWords) where

import Prelude hiding (readFile)
import System.IO (hPutStr, stderr) 
import System.IO.Error (ioeGetErrorString, ioeGetLocation)
import Control.Exception (IOException, catch)
import Control.Monad (forM, foldM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.ByteString.Char8 (readFile, unpack)
import Text.Regex.Posix ((=~))
import Data.List (nub)

import Types
import Classify

train :: WMap -> FilePath -> MsgType -> IO (Maybe WMap)
train wm path t = do
  isDirectory <- doesDirectoryExist path
  if isDirectory then do fs <- getRecursiveContents path
                         foldM (\acc f -> trainFile (fromJust acc) f t 
                                          `catch` handler) (Just wm) fs
  else trainFile wm path t `catch` handler

trainFile :: WMap -> FilePath -> MsgType -> IO (Maybe WMap)
trainFile wm path t = do
  ws <- getWords path
  return $ Just (trainWMap wm ws t)

getWords :: FilePath -> IO [String]
getWords p = do
  str <- readFile p
  let lineBreakPat = "^\\s*$"
      (before, _, after) = unpack str =~ lineBreakPat :: (String,String,String)
      str' = if null after then before else after
      --just the actual words
      wordPat = "([a-zA-Z]\\w*)"
      (_, _, _, ws) = str' =~ wordPat :: (String, String, String, [String])
  return $ nub ws
  
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

handler :: IOException -> IO (Maybe a)
handler e = do 
  let err = show (e :: IOException)
  hPutStr stderr ("Warning: Couldn't open file: " ++ err)
  return Nothing
