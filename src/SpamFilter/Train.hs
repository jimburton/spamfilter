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
module SpamFilter.Train
    (train, trainWMap, getWords)
    where

import           Control.Exception     (IOException, catch)
import           Control.Monad         (foldM, forM)
import           Data.ByteString.Char8 (readFile, unpack)
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import           Prelude               hiding (readFile)
import           System.Directory      (doesDirectoryExist,
                                        getDirectoryContents)
import           System.FilePath       ((</>))
import           System.IO             (hPutStr, stderr)
import           Text.Regex.Posix      ((=~))
import           SpamFilter.Classify ( getWordFeat )
import           SpamFilter.Types
    ( WMap
    , WordFeature(hamCount, spamCount, pk)
    , MsgType(..) )

{-| Takes a WMap reflecting the current state of the filter (i.e. what is currently
known about spam and ham), the path to some spam or ham message(s) and a MsgType to
say whether the message(s) should be added as spam or ham. Returns an updated WMap
reflecting the new state of the filter.
-}
train :: WMap -> FilePath -> MsgType -> IO (Maybe WMap)
train wm path t = do
  isDirectory <- doesDirectoryExist path
  if isDirectory then do fs <- getRecursiveContents path
                         foldM (\acc f -> trainFile (fromJust acc) f t
                                          `catch` handler) (Just wm) fs
  else trainFile wm path t `catch` handler

{-| Train the filter on an individual file containing an email.
-}
trainFile :: WMap -> FilePath -> MsgType -> IO (Maybe WMap)
trainFile wm path t = do
  ws <- getWords path
  return $ Just (trainWMap wm ws t)

{-| Update the ham or spam counts in the WMap for this list of words. -}
trainWMap :: WMap -> [String] -> MsgType -> WMap
trainWMap m s t = foldl (incrementCount t) m s

{-| Update the ham or spam counts in the WMap for this particular word. -}
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
incrementCount Unclear _ _ = error "We don't count unclears"

{-| Takes the path to an email and returns just the words in the body of the email.
-}
getWords :: FilePath -> IO [String]
getWords p = do
  str <- readFile p
  let lineBreakPat = "^\\s*$"
      (before, _, after) = unpack str =~ lineBreakPat :: (String,String,String)
      str' = if null after then before else after
      --just the actual words
      wordPat       = "([a-zA-Z]+)"
      (_, _, _, ws) = str' =~ wordPat :: (String, String, String, [String])
  -- return $ nub (words str')
  -- return $ nub ws
  return ws

{-| Collect all filepaths within a given directory, recursively drilling down as
necessary.
-}
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

{-| Handles exceptions.
-}
handler :: IOException -> IO (Maybe a)
handler e = do
  let err = show (e :: IOException)
  hPutStr stderr ("Warning: Couldn't open file: " ++ err)
  return Nothing
