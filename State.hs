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
    --
    , Stats (..)
    , Thread (..)
    , P.Post (..)
    , P.PostContent (..)
    , statScore
    , statPercent
    , P.children
    , P.recentChildren
    , P.threadLength
    -- Acidic
    ---- Stats
    , PlayerCount (..)
    , NewPlayer (..)
    , AllStats (..)
    , SearchStats (..)
    , PlayerStats (..)
    ---- Voting
    , GetVote (..)
    , ProcessVote (..)
    ---- Posts
    , PlayerThreads (..)
    , GetThread (..)
    , NewTopPost (..)
    , NewReply (..)
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
import Control.Monad (when)

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
playerCount = queryer playerStore R.playerCount

---------------

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

    when (R.playerExists n players) $ do
        let (ips', prevVote) = V.vote ip n v ips
        let thisVoteMod = applyVote v prevVote n
        let players' = thisVoteMod players
        put state {playerStore = players', ipStore =  ips'}

------
statPercent :: Stats -> Int
statPercent p = f u d
            where u = statUpvotes p
                  d = statDownvotes p
                  f 0 0 = 0
                  f _ 0 = 100
                  f _ _ = (u * 100) `div` (u + d)
                

data Stats = Stats { statName :: Name
                   , statUpvotes :: Int
                   , statDownvotes :: Int
                   , statRank :: Int
                   , statComments :: Int
                   } deriving (Typeable, Eq)

statScore ::  Stats -> Int
statScore stats = statUpvotes stats - statDownvotes stats

$(deriveSafeCopy 0 'base ''Stats)

addCommentCount ::  Int -> R.RankStats -> Stats
addCommentCount ncom rs = Stats { statName = R.rankName rs
                                            , statUpvotes = R.upvotes rs
                                            , statDownvotes = R.downvotes rs
                                            , statRank = R.rank rs
                                            , statComments = ncom
                                            }


playerStats :: Name -> Query AppState (Maybe Stats)
playerStats name = do
    state <- ask
    let rankStats = R.getStats name . playerStore $ state
    let count = P.commentCount name . postStore $ state -- Is this lazy?
    let stats = fmap (addCommentCount count) rankStats
    return stats

allStats :: Query AppState [Stats]
allStats = do
    state <- ask
    let rankStats = R.allStats . playerStore $ state
    let getCount rs = P.commentCount (R.rankName rs) . postStore $ state
    let counts = map getCount rankStats
    let stats = zipWith addCommentCount counts rankStats
    return stats

searchStats :: Text -> Query AppState [Stats]
searchStats x = do
    state <- ask
    let rankStats = R.searchStats x . playerStore $ state
    let getCount rs = P.commentCount (R.rankName rs) . postStore $ state
    let counts = map getCount rankStats
    let stats = zipWith addCommentCount counts rankStats
    return stats

------------------------------

playerThreads ::  Name -> Query AppState [Thread]
playerThreads name = queryer postStore $ P.playerThreads name

getThread ::  Int -> Query AppState (Maybe Thread)
getThread num = queryer postStore $ P.getThread num

newTopPost :: Name -> P.PostContent -> Update AppState (Maybe Int)
newTopPost name content = do
    state <- get
    let exists = R.playerExists name . playerStore $ state
    if exists
        then do
            let (posts, num) = P.newTopPostNC name content . postStore $ state
            put $ setPostStore posts state
            return $ Just num
        else return Nothing

newReply ::  Int -> P.PostContent -> Update AppState (Maybe Int)
newReply parNum content = updaterMaybe postStore setPostStore $ P.newReply parNum content

$(makeAcidic ''AppState [ 'newPlayer
                        , 'getVote
                        , 'processVote
                        , 'playerCount
                        , 'allStats
                        , 'searchStats
                        , 'playerStats
                        , 'playerThreads
                        , 'getThread
                        , 'newTopPost
                        , 'newReply
                        ])
