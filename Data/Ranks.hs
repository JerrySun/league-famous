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
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Map as M
import Data.Text (Text)

data Player = Player { playerName :: Name
                     , playerUpvotes :: Int
                     , playerDownvotes :: Int } deriving (Eq, Typeable)

type Name = Text

data PlayerStore = PlayerStore {unwrapMap :: M.Map Name Player} deriving (Typeable)

$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''PlayerStore)


empty :: PlayerStore
empty = PlayerStore M.empty

setPlayer :: Player -> PlayerStore -> PlayerStore
setPlayer p (PlayerStore s) = PlayerStore $ M.insert (playerName p) p s

newPlayer name ranks = case M.lookup name (unwrapMap ranks) of
                           Nothing -> setPlayer (Player name 0 0) ranks
                           Just _ -> ranks

getPlayer n = M.lookup n . unwrapMap

allPlayers = M.elems . unwrapMap
