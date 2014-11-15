module Types where

import qualified Data.Map as M

data MsgType = Ham | Unclear | Spam deriving (Show, Eq)

data WordFeature = WordFeature {word :: String,
                                hamCount :: Int,
                                spamCount :: Int}
                   deriving (Show, Eq)

type WMap = (Int, Int, M.Map String WordFeature) --(total hams, total spams, map)
