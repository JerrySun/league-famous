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

data Player = Player { playerName :: Name
                     , playerUpvotes :: Int
                     , playerDownvotes :: Int } deriving (Eq, Typeable)

type Name = Text

data PlayerStore = PlayerStore {unwrapMap :: M.Map Name Player} deriving (Typeable)

$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''PlayerStore)

----
data Vote = Up | Down | Neutral deriving (Show, Eq, Typeable)

data IP = IPv4 Word32 | IPv6 (Word32, Word32, Word32, Word32) deriving (Ord, Eq, Typeable)

data IPStore = IPStore (M.Map IP (M.Map Name Vote)) deriving (Typeable)

$(deriveSafeCopy 0 'base ''Vote)
$(deriveSafeCopy 0 'base ''IP)
$(deriveSafeCopy 0 'base ''IPStore)

----
data AppState = AppState { getPlayerStore :: PlayerStore
                         , getIPStore :: IPStore } deriving (Typeable)

$(deriveSafeCopy 0 'base ''AppState)

emptyState ::  AppState
emptyState = AppState emptyPlayerS emptyIPS


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

emptyIPS :: IPStore
emptyIPS = IPStore M.empty

vote :: IP -> Name -> Vote -> IPStore -> (IPStore, Vote)
vote ip n v (IPStore s) = 
    case M.lookup ip s of
        Nothing      -> ( IPStore $ M.insert ip (M.singleton n v) s
                        , Neutral )
        Just votemap -> ( IPStore $ M.insert ip (M.insert n v votemap) s
                        , fromMaybe Neutral (M.lookup n votemap))

updateVote :: IP -> Name -> Vote -> Update AppState Vote
updateVote ip n v = ipUpdater $ vote ip n v

ipUpdater :: (IPStore -> (IPStore, a)) -> Update AppState a
ipUpdater f = do AppState players ips <- get
                 let (ips', result) = f ips
                 put $ AppState players ips'
                 return result

ipQueryer :: (IPStore -> a) -> Query AppState a
ipQueryer f = do AppState _ ips <- ask
                 let result = f ips
                 return result

getVote :: IP -> Name -> Query AppState Vote
getVote ip name = ipQueryer f
                  where f (IPStore s) = fromMaybe Neutral $ M.lookup ip s >>= M.lookup name
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
    AppState (PlayerStore players) (IPStore ips) <- get

    case M.lookup n players of
        Nothing -> return False
        Just player -> do
            let (ips', prevVote) = case M.lookup ip ips of
                                    Nothing ->
                                        (M.insert ip (M.singleton n v) ips, Neutral)
                                    Just votemap ->
                                        (M.insert ip (M.insert n v votemap) ips, fromMaybe Neutral (M.lookup n votemap))
            let player' = applyVote v prevVote player
            let players' = M.insert n player' players
            put $ AppState (PlayerStore players') (IPStore ips')
            return True

$(makeAcidic ''AppState ['newPlayer, 'setPlayer, 'getPlayer, 'allPlayers, 'upvote, 'downvote, 'updateVote, 'getVote, 'processVote])
