{-# LANGUAGE OverloadedStrings #-}
module DBHelper (putWMap, getWMap) where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Map as M

import Types (WordFeature(..), WordRow(..), WMap(..), MsgType(..))

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

updateCount :: Connection -> Int -> MsgType -> IO ()
updateCount conn count t = do
  let c = case t of 
            Ham -> "H"
            Spam -> "S"
  execute conn "UPDATE counts SET count=? WHERE type = ?" 
              (count :: Int, c :: String)

updateWord conn WordFeature {hamCount = hs, spamCount = ss, pk = Just i} = 
    execute conn "UPDATE words SET hamcount=?, spamcount=? WHERE id = ?" 
                (hs :: Int, ss :: Int, i :: Int) 

insertWord conn WordFeature {word = w, hamCount = hs, spamCount = ss} = 
    execute conn "INSERT INTO words (word, hamcount, spamcount) VALUES (?,?,?)" 
                (w :: String, hs :: Int, ss :: Int) 
   
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

