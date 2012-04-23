{-# LANGUAGE DeriveDataTypeable #-}
module State where

import Data.Acid
import Data.SafeCopy
import qualified Data.Map as M
import Data.Typeable
import Control.Monad.State (gets, put)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import Prelude
import Data.Word (Word32)
import Data.Maybe (fromMaybe)

data Player = Player { playerName :: Name
                     , playerUpvotes :: Int
                     , playerDownvotes :: Int } deriving (Typeable)

playerRatio :: Player -> Float
playerRatio p = let up = playerUpvotes p
                    down = playerDownvotes p
                    fI = fromIntegral
                in case up `compare` down of
                    GT -> fI up / fI down
                    LT -> fI down / fI up * (-1)
                    EQ -> 1

type Name = Text

data PlayerStore = PlayerStore {unwrapMap :: M.Map Name Player} deriving (Typeable)

emptyStore ::  PlayerStore
emptyStore = PlayerStore M.empty

$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''PlayerStore)

setPlayer :: Player -> Update PlayerStore ()
setPlayer p = gets (M.insert (playerName p) p . unwrapMap) >>= put . PlayerStore

getPlayer :: Name -> Query PlayerStore (Maybe Player)
getPlayer n = asks $ M.lookup n . unwrapMap

allPlayers :: Query PlayerStore [Player]
allPlayers = asks $ M.elems . unwrapMap

upvote ::  Name -> Update PlayerStore (Maybe Player)
upvote n = do
    player <- runQuery $ getPlayer n
    case player of
        Just (Player name up down) -> let new = Player name (up + 1) down
                                      in setPlayer new >> return (Just new)
        Nothing -> return Nothing
                                      
downvote ::  Name -> Update PlayerStore (Maybe Player)
downvote n = do
    player <- runQuery $ getPlayer n
    case player of
        Just (Player name up down) -> let new = Player name up (down + 1)
                                      in setPlayer new >> return (Just new)
        Nothing -> return Nothing
                                    
$(makeAcidic ''PlayerStore ['setPlayer, 'getPlayer, 'allPlayers, 'upvote, 'downvote])

data Vote = Up | Down | Neutral deriving (Typeable)

data IP = IPv4 Word32 | IPv6 (Word32, Word32, Word32, Word32) deriving (Ord, Eq, Typeable)

data IPStore = IPStore (M.Map IP (M.Map Name Vote)) deriving (Typeable)

vote :: IP -> Name -> Vote -> IPStore -> (IPStore, Vote)
vote ip n v (IPStore s) = 
    case M.lookup ip s of
        Nothing      -> ( IPStore $ M.insert ip (M.singleton n v) s
                        , Neutral )
        Just votemap -> ( IPStore $ M.insert ip (M.insert n v votemap) s
                        , fromMaybe Neutral (M.lookup n votemap))
