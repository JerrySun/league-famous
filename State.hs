{-# LANGUAGE DeriveDataTypeable #-}
module State 
    ( IP (..)
    , AppState
    , Player (..)
    , Vote (..)
    , Name
    , emptyState
    -- Acidic
    , AllPlayers (..)
    , GetVote (..)
    , ProcessVote (..)
    , NewPlayer (..)
    , GetPlayer (..)
    ) where

import Data.Acid
import Data.SafeCopy
import qualified Data.Map as M
import Data.Typeable
import Control.Monad.State (get, gets, put)
import Control.Monad.Reader (ask, asks)
import Data.Text (Text)
import Prelude
import Data.Word (Word32)
import Data.Maybe (fromMaybe)
import Data.Ranks
import Data.VoteHistory (IPStore, IP, Vote(..))
import qualified Data.VoteHistory as VH

----
----
data AppState = AppState { getPlayerStore :: PlayerStore
                         , getIPStore :: IPStore } deriving (Typeable)

$(deriveSafeCopy 0 'base ''AppState)

emptyState ::  AppState
emptyState = AppState emptyPlayerS VH.empty


emptyPlayerS ::PlayerStore
emptyPlayerS = PlayerStore M.empty

playerRatio :: Player -> Float
playerRatio p = let up = playerUpvotes p
                    down = playerDownvotes p
                    fI = fromIntegral
                in case up `compare` down of
                    GT -> fI up / fI down
                    LT -> fI down / fI up * (-1)
                    EQ -> 1

playerUpdater :: (PlayerStore -> (PlayerStore, a)) -> Update AppState a
playerUpdater f = do AppState players ips <- get
                     let (players', result) = f players
                     put $ AppState players' ips
                     return result

playerQueryer :: (PlayerStore -> a) -> Query AppState a
playerQueryer f = do AppState players _ <- ask
                     let result = f players
                     return result

setPlayer' :: Player -> PlayerStore -> (PlayerStore, ())
setPlayer' p (PlayerStore s) = (PlayerStore $ M.insert (playerName p) p s, ())

setPlayer :: Player -> Update AppState ()
setPlayer p = playerUpdater $ setPlayer' p

newPlayer name = playerUpdater $ \ranks ->
    case M.lookup name (unwrapMap ranks) of
        Nothing -> setPlayer' (Player name 0 0 ) ranks
        Just _ -> (ranks, ())

getPlayer :: Name -> Query AppState (Maybe Player)
getPlayer n = playerQueryer $ M.lookup n . unwrapMap

allPlayers :: Query AppState [Player]
allPlayers = playerQueryer $ M.elems . unwrapMap

upvote ::  Name -> Update AppState (Maybe Player)
upvote n = do
    player <- runQuery $ getPlayer n
    case player of
        Just (Player name up down) -> let new = Player name (up + 1) down
                                      in setPlayer new >> return (Just new)
        Nothing -> return Nothing
                                      
downvote ::  Name -> Update AppState (Maybe Player)
downvote n = do
    player <- runQuery $ getPlayer n
    case player of
        Just (Player name up down) -> let new = Player name up (down + 1)
                                      in setPlayer new >> return (Just new)
        Nothing -> return Nothing
                                    

----------------
----------------

ipUpdater :: (IPStore -> (IPStore, a)) -> Update AppState a
ipUpdater f = do AppState players ips <- get
                 let (ips', result) = f ips
                 put $ AppState players ips'
                 return result

ipQueryer :: (IPStore -> a) -> Query AppState a
ipQueryer f = do AppState _ ips <- ask
                 let result = f ips
                 return result

---------------
updateVote :: IP -> Name -> Vote -> Update AppState Vote
updateVote ip n v = ipUpdater $ VH.vote ip n v


getVote :: IP -> Name -> Query AppState Vote
getVote ip name = ipQueryer $ VH.getVote ip name
----------------
----------------

applyVote new old p@(Player n u d) = f new old
                                       where f Up Up      = p
                                             f Up Neutral = Player n (u + 1) d
                                             f Up Down    = Player n (u + 1) (d - 1)
                                             f Down Down    = p
                                             f Down Neutral = Player n u (d + 1)
                                             f Down Up      = Player n (u - 1) (d + 1)
                                             f Neutral Neutral = p
                                             f Neutral Up      = Player n (u - 1) d
                                             f Neutral Down    = Player n u (d - 1)

processVote :: IP -> Name -> Vote -> Update AppState Bool
processVote ip n v = do
    AppState (PlayerStore players) ips <- get

    case M.lookup n players of
        Nothing -> return False
        Just player -> do
            let (ips', prevVote) = VH.vote ip n v ips
            let player' = applyVote v prevVote player
            let players' = M.insert n player' players
            put $ AppState (PlayerStore players') ips'
            return True

$(makeAcidic ''AppState ['newPlayer, 'setPlayer, 'getPlayer, 'allPlayers, 'upvote, 'downvote, 'updateVote, 'getVote, 'processVote])
