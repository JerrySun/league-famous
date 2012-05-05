{-# LANGUAGE DeriveDataTypeable #-}
module Data.VoteHistory
    ( IP (..)
    , Vote (..)
    , IPStore
    , vote
    , getVote
    , empty
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Map as M
import Data.Word (Word32)
import Data.Name (Name)
import Data.Maybe (fromMaybe)
import Data.Aeson
import Control.Monad (mzero)

data Vote = Up | Down | Neutral deriving (Show, Eq, Typeable)

data IP = IPv4 Word32 | IPv6 (Word32, Word32, Word32, Word32) deriving (Ord, Eq, Typeable)

data IPStore = IPStore { unwrapMap :: M.Map IP (M.Map Name Vote) } deriving (Typeable)

$(deriveSafeCopy 0 'base ''Vote)
$(deriveSafeCopy 0 'base ''IP)
$(deriveSafeCopy 0 'base ''IPStore)

instance FromJSON Vote where
    parseJSON (String t) = case t of
                               "up"      -> return Up
                               "down"    -> return Down
                               "neutral" -> return Neutral
                               _         -> mzero
    parseJSON _          = mzero

empty :: IPStore
empty = IPStore M.empty


-- Update IP's vote and also get the previous one
vote :: IP -> Name -> Vote -> IPStore -> (IPStore, Vote)
vote ip n v (IPStore s) = 
    case M.lookup ip s of
        Nothing      -> ( IPStore $ M.insert ip (M.singleton n v) s
                        , Neutral )
        Just votemap -> ( IPStore $ M.insert ip (M.insert n v votemap) s
                        , fromMaybe Neutral (M.lookup n votemap))

getVote :: IP -> Name -> IPStore -> Vote
getVote ip name (IPStore s) = fromMaybe Neutral $ M.lookup ip s >>= M.lookup name
