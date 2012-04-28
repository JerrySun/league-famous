{-# LANGUAGE DeriveDataTypeable #-}
module Data.Ranks
    ( Player (..)
    , Name
    , PlayerStore 
    , empty
    , setPlayer
    , newPlayer
    , getPlayer
    , allPlayers
    , validName
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T 
import qualified Data.Char as C

data Player = Player { playerName :: Name
                     , playerUpvotes :: Int
                     , playerDownvotes :: Int } deriving (Eq, Typeable)

type Name = Text

data PlayerStore = PlayerStore {unwrapMap :: M.Map Name Player} deriving (Typeable)

$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''PlayerStore)

validName :: Text -> Bool
validName n = conditions n
              where conditions = (<= 16) . T.length
                                 &&* T.all C.isAlphaNum

infixr 3 &&*
(&&*) ::  (t -> Bool) -> (t -> Bool) -> t -> Bool
(&&*) g h x = g x && h x

infixr 3 ||*
(||*) ::  (t -> Bool) -> (t -> Bool) -> t -> Bool
(||*) g h x = g x || h x

empty :: PlayerStore
empty = PlayerStore M.empty

setPlayer :: Player -> PlayerStore -> PlayerStore
setPlayer p (PlayerStore s) = PlayerStore $ M.insert (playerName p) p s

newPlayer name ranks = case validName name of 
                           True -> case M.lookup name (unwrapMap ranks) of
                                       Nothing -> setPlayer (Player name 0 0) ranks
                                       Just _ -> ranks
                           False -> ranks

getPlayer n = M.lookup n . unwrapMap

allPlayers = M.elems . unwrapMap
