{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Map as M

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

main :: IO ()
main = do
  conn <- open "spam.db"
  execute conn "INSERT INTO words (str) VALUES (?)"
    (Only ("test string 2" :: String))
  r <- query_ conn "SELECT * from words" :: IO [TestField]
  mapM_ print r
  close conn
