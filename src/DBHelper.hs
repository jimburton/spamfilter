{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  DBHelper.hs
Description :  Database helper for the spamfilter program
Copyright   :  (c) Jim Burton
License     :  MIT

Maintainer  :  j.burton@brighton.ac.uk
Stability   :  provisional 
Portability :  portable 

Module which supports interaction with the sqlite database which stores the 
results of training the filter. Uses the library sqlite-simple to access an
SQLite database.
-}
module DBHelper (putWMap, getWMap) where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Map as M

import Types (WordFeature(..), WordRow(..), WMap(..), MsgType(..))

{-| Store the contents of a WMap in the database. -}
putWMap :: WMap -> IO ()
putWMap (hc, sc, m) = do
  conn <- open "../spam.db"
  withTransaction conn $ do
    let (is, us) = foldl (\(is,us) (_, feat@WordFeature{pk=mi})
                          -> maybe (feat:is,us) (const (is,feat:us)) mi) 
                   ([], []) $ M.toList m
    mapM_ (updateWord conn) us
    mapM_ (insertWord conn) is
    updateCount conn hc Ham
    updateCount conn sc Spam

{-| Update a row in the counts table. -}
updateCount :: Connection -> Int -> MsgType -> IO ()
updateCount conn count t = do
  let c = case t of 
            Ham -> "H"
            Spam -> "S"
  execute conn "UPDATE counts SET count=? WHERE type = ?" 
              (count :: Int, c :: String)

{-| Update a row in the words table. -}
updateWord conn WordFeature {hamCount = hs, spamCount = ss, pk = Just i} = 
    execute conn "UPDATE words SET hamcount=?, spamcount=? WHERE id = ?" 
                (hs :: Int, ss :: Int, i :: Int) 

{-| Insert a word that we haven't seen before -}
insertWord conn WordFeature {word = w, hamCount = hs, spamCount = ss} = 
    execute conn "INSERT INTO words (word, hamcount, spamcount) VALUES (?,?,?)" 
                (w :: String, hs :: Int, ss :: Int) 
   
{-| Pull the contents of the database into a WMap. -}
getWMap :: IO WMap
getWMap = do 
  conn <- open "../spam.db"
  r <- query_ conn "SELECT * from words" :: IO [WordRow]
  let wMap = foldl (\ht (WordRow i w hs ss) -> M.insert w 
                    WordFeature {word = w,
                                 hamCount = hs,
                                 spamCount = ss,
                                 pk = Just i} ht) 
             M.empty r
  [hc] <- query_ conn "SELECT count from counts where type = 'H'" :: IO [Only Int]
  [sc] <- query_ conn "SELECT count from counts where type = 'S'" :: IO [Only Int]
  close conn
  return (fromOnly hc, fromOnly sc, wMap)

