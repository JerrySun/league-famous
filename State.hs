{-# LANGUAGE DeriveDataTypeable #-}
module State 
    -- re-exports
    ( IP (..)
    , Vote (..)
    , Name (..)
    , N.validName
    -- The master state
    , AppState
    , emptyState
    -- Acidic
    ---- Stats
    , PlayerCount (..)
    , NewPlayer (..)
    ---- Voting
    , GetVote (..)
    , ProcessVote (..)
    ---- Posts
    ) where

import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Data.Text (Text)
import Prelude
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)

import Data.VoteHistory (IPStore, IP, Vote(..))
import qualified Data.VoteHistory as V

import Data.Ranks (PlayerStore)
import qualified Data.Ranks as R

import Data.Posts (Thread (..), PostStore)
import qualified Data.Posts as P

import Data.Name (Name (..))
import qualified Data.Name as N

----
----
data AppState = AppState { playerStore :: PlayerStore
                         , ipStore :: IPStore
                         , postStore :: PostStore } deriving (Typeable)

$(deriveSafeCopy 0 'base ''AppState)

setPlayerStore ::  PlayerStore -> AppState -> AppState
setPlayerStore ps  state = state { playerStore = ps}

setIPStore ::  IPStore -> AppState -> AppState
setIPStore     ips state = state { ipStore = ips}

setPostStore ::  PostStore -> AppState -> AppState
setPostStore  posts state = state { postStore = posts }

queryer ::  (AppState -> s) -> (s -> b) -> Query AppState b
queryer getter = flip fmap $ fmap getter ask

updater :: (AppState -> s) -> (s -> AppState -> AppState) -> (s -> (s, b)) -> Update AppState b
updater getter setter f = do
    state <- get
    let (x', result) = f $ getter state
    put $ setter x' state
    return result

updaterMaybe :: (AppState -> s) -> (s -> AppState -> AppState) -> (s -> Maybe (s, b)) -> Update AppState (Maybe b)
updaterMaybe getter setter f = do
    state <- get
    case f $ getter state of
        Just (x', result) -> do put $ setter x' state
                                return $ Just result
        Nothing -> return Nothing

updater_ :: (AppState -> s) -> (s -> AppState -> AppState) -> (s -> s) -> Update AppState ()
updater_ getter setter f = uncurry setter . ((f . getter) &&& id) <$> get >>= put

emptyState ::  AppState
emptyState = AppState R.empty V.empty P.empty

----

newPlayer ::  Name -> Update AppState ()
newPlayer name = updater_ playerStore setPlayerStore $ R.newPlayer name

playerCount ::  Query AppState Int
playerCount = queryer playerStore $ R.playerCount

---------------

updateVote :: IP -> Name -> Vote -> Update AppState Vote
updateVote ip n v = updater ipStore setIPStore $ V.vote ip n v


getVote :: IP -> Name -> Query AppState Vote
getVote ip name = queryer ipStore $ V.getVote ip name

----------------
----------------

applyVote ::  Vote -> Vote -> Name -> PlayerStore -> PlayerStore
applyVote new old name store = fromMaybe store $ f new old store
    where f Up      Up      = R.voteMod name 0  0
          f Up      Neutral = R.voteMod name 1  0
          f Up      Down    = R.voteMod name 1  (-1)
          f Down    Down    = R.voteMod name 0  0
          f Down    Neutral = R.voteMod name 0  1
          f Down    Up      = R.voteMod name (-1) 1
          f Neutral Neutral = R.voteMod name 0  0
          f Neutral Up      = R.voteMod name (-1) 0
          f Neutral Down    = R.voteMod name 0  (-1)

processVote :: IP -> Name -> Vote -> Update AppState ()
processVote ip n v = do
    state <- get
    let players = playerStore state
    let ips     = ipStore state

    if R.playerExists n players
        then do
            let (ips', prevVote) = V.vote ip n v ips
            let thisVoteMod = applyVote v prevVote n
            let players' = thisVoteMod players
            put state {playerStore = players', ipStore =  ips'}
        else
            return ()

------


$(makeAcidic ''AppState [ 'newPlayer
                        , 'updateVote
                        , 'getVote
                        , 'processVote
                        , 'playerCount
                        ])
