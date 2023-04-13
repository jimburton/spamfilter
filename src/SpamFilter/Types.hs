{- |
Module      :  Types.hs
Description :  Datatypes used by the spamfilter program
Copyright   :  (c) Jim Burton
License     :  MIT

Maintainer  :  jimburton1@gmail.com
Stability   :  provisional
Portability :  portable

Definitions of the types used in the program.
-}
module SpamFilter.Types where

import qualified Data.Map as M
import Database.SQLite.Simple.FromRow ( field, FromRow(..) )

-- | A type used to classify messages.
data MsgType = Ham | Unclear | Spam deriving (Show, Read)

-- | A WordFeature is a word along with the number of times we have
-- | seen it occuring in ham and spam messages, respectively. The pk field
-- | is the primary key from the database, set to Nothing for WordFeatures
-- | that haven't been stored yet (i.e. that were the result of the last
-- | training exercise).
data WordFeature = WordFeature { word      :: String
                               , hamCount  :: Int
                               , spamCount :: Int
                               , pk        :: Maybe Int}
                   deriving (Show, Eq)

-- | A WMap is the current state of the filter. The first two fields are the total
-- | number of ham and spam messages seen by this filter, and the map allows us to look
-- | up the WordFeature associated with a given word.
type WMap = (Int, Int, M.Map String WordFeature) --(total hams, total spams, map)

-- | A WordRow represents a row from the database. The sqlite-simple library will
-- | automatically marshal rows of data into WordRows for us.
data WordRow = WordRow Int String Int Int deriving (Show)

-- | For our database library to perform its magic, the types that we want to
-- | populated with data resulting from a query need to be instance of FromRow. This
-- | just tells sqlite-simple how to slot data into values of this type.
instance FromRow WordRow where
  fromRow = WordRow <$> field <*> field <*> field <*> field
