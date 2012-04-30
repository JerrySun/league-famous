{-# LANGUAGE DeriveDataTypeable #-}
module State 
    -- re-exports
    ( IP (..)
    , Player (..)
    , Vote (..)
    , Name
    , R.validName
    , R.playerScore
    -- The master state
    , AppState
    , emptyState
    -- Acidic
    , AllPlayers (..)
    , GetVote (..)
    , ProcessVote (..)
    , NewPlayer (..)
    , GetPlayer (..)
    , PlayerCount (..)
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
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))

import Data.VoteHistory (IPStore, IP, Vote(..))
import qualified Data.VoteHistory as V

import Data.Ranks (PlayerStore, Player (..), Name)
import qualified Data.Ranks as R

----
----
data AppState = AppState { playerStore :: PlayerStore
                         , ipStore :: IPStore } deriving (Typeable)

$(deriveSafeCopy 0 'base ''AppState)

setPlayerStore ps  state = state { playerStore = ps}
setIPStore     ips state = state { ipStore = ips}

queryer ::  (AppState -> s) -> (s -> b) -> Query AppState b
queryer getter = (flip fmap) (fmap getter ask)

updater :: (AppState -> s) -> (s -> AppState -> AppState) -> (s -> (s, b)) -> Update AppState b
updater getter setter f = do
    state <- get
    let (x', result) = f $ getter state
    put $ setter x' state
    return result

updater' :: (AppState -> s) -> (s -> AppState -> AppState) -> (s -> s) -> Update AppState ()
updater' getter setter f = uncurry setter . ((f . getter) &&& id) <$> get >>= put

emptyState ::  AppState
emptyState = AppState R.empty V.empty

----

playerUpdater = updater playerStore setPlayerStore
playerUpdater' = updater' playerStore setPlayerStore
playerQueryer = queryer playerStore

----

newPlayer name = playerUpdater' $ R.newPlayer name

getPlayer :: Name -> Query AppState (Maybe Player)
getPlayer n = playerQueryer $ R.getPlayer n

allPlayers :: Query AppState [Player]
allPlayers = playerQueryer $ R.allPlayers

playerCount = playerQueryer $ R.playerCount

----------------
----------------

ipUpdater :: (IPStore -> (IPStore, a)) -> Update AppState a
ipUpdater = updater ipStore setIPStore

ipQueryer :: (IPStore -> a) -> Query AppState a
ipQueryer = queryer ipStore



---------------
updateVote :: IP -> Name -> Vote -> Update AppState Vote
updateVote ip n v = ipUpdater $ V.vote ip n v


getVote :: IP -> Name -> Query AppState Vote
getVote ip name = ipQueryer $ V.getVote ip name
----------------
----------------

applyVote new old p =
    f new old
    where f Up      Up      = R.voteMod p 0  0
          f Up      Neutral = R.voteMod p 1  0
          f Up      Down    = R.voteMod p 1  (-1)
          f Down    Down    = R.voteMod p 0  0
          f Down    Neutral = R.voteMod p 0  1
          f Down    Up      = R.voteMod p (-1) 1
          f Neutral Neutral = R.voteMod p 0  0
          f Neutral Up      = R.voteMod p (-1) 0
          f Neutral Down    = R.voteMod p 0  (-1)

processVote :: IP -> Name -> Vote -> Update AppState Bool
processVote ip n v = do
    state <- get
    let players = playerStore state
    let ips     = ipStore state

    case R.getPlayer n players of
        Nothing -> return False
        Just player -> do
            let (ips', prevVote) = V.vote ip n v ips
            let thisVoteMod = applyVote v prevVote player
            let players' = thisVoteMod players
            put $ AppState {playerStore = players', ipStore =  ips'}
            return True

$(makeAcidic ''AppState [ 'newPlayer
                        , 'getPlayer
                        , 'allPlayers
                        , 'updateVote
                        , 'getVote
                        , 'processVote
                        , 'playerCount
                        ])
