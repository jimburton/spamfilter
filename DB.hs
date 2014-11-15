{-# LANGUAGE OverloadedStrings #-}
module DBHelper where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Map as M

data WordRow = WordRow Int String Float deriving (Show)

data MsgType = Ham | Unclear | Spam deriving (Show, Eq)

instance FromRow WordRow where
  fromRow = WordRow <$> field <*> field <*> field

writeWords conn m = do
  let (is, us) = foldl (\(is,us) (k,(s,mi)) 
                        -> maybe ((k,s,mi):is,us) (const (is,(k,s,mi):us)) mi) 
                 ([], []) $ M.toList m
  mapM_ (updateWord conn) us
  mapM_ (insertWord conn) is

updateWord conn (word, score, (Just i)) = 
    execute conn "UPDATE words SET score=? WHERE id = ?" 
                (score :: Float, i :: Int) 

insertWord conn (word, score, _) = 
    execute conn "INSERT INTO words (word, score) VALUES (?,?)" 
                (word :: String, score :: Float) 
   
selectWords conn = query_ conn "SELECT * from words" :: IO [WordRow]

main :: IO ()
main = do
  conn <- open "spam.db"
  --execute_ conn "INSERT INTO words (word, score) VALUES ('spam', 0.7)"
  r <- selectWords conn
  let wMap = foldl (\ht (WordRow i w s) -> M.insert w (s, Just i) ht) 
             M.empty r 
      wMap' = let m = M.lookup "Bananas" wMap in
              case m of 
                Nothing -> M.insert "Bananas" (0.7, Nothing) wMap
                Just (s, mi) -> M.insert "Bananas" (s+0.01, mi) wMap
  print wMap'
  mapM_ print r
  writeWords conn wMap' 
  q <- selectWords conn
  mapM_ print q
  close conn
