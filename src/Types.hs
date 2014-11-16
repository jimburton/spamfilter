module Types where

import Control.Applicative
import qualified Data.Map as M
import Database.SQLite.Simple.FromRow

data MsgType = Ham | Unclear | Spam deriving (Show, Read)

data WordFeature = WordFeature {word :: String,
                                hamCount :: Int ,
                                spamCount :: Int,
                                pk :: Maybe Int}
                   deriving (Show, Eq)

type WMap = (Int, Int, M.Map String WordFeature) --(total hams, total spams, map)

data WordRow = WordRow Int String Int Int deriving (Show)

instance FromRow WordRow where
  fromRow = WordRow <$> field <*> field <*> field <*> field
