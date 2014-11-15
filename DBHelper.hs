{-# LANGUAGE OverloadedStrings #-}
module DBHelper where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Map as M

data WordRow = WordRow Int String Float deriving (Show)

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

