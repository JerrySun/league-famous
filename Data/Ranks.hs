{-# LANGUAGE DeriveDataTypeable #-}
module Data.Ranks
    ( RankStats (..)
    , PlayerStore 
    , empty
    , newPlayer
    , voteMod
    , getStats
    , allStats
    , searchStats
    , playerCount
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.Maybe (fromMaybe, fromJust)
import Data.Name
import qualified Data.Text as T

data StoredPlayer = StoredPlayer { playerName :: Name
                                 , playerUpvotes :: Int
                                 , playerDownvotes :: Int
                                 } deriving (Eq, Typeable)

$(deriveSafeCopy 0 'base ''StoredPlayer)

playerScore :: StoredPlayer -> Int
playerScore p = playerUpvotes p - playerDownvotes p 

data PlayerStore = PlayerStore { nameMap :: M.Map Name StoredPlayer
                               , scoreMap :: I.IntMap (M.Map Name StoredPlayer) }
                               deriving (Typeable)

setNameMap nm store = store { nameMap = nm }
setScoreMap sm store = store { scoreMap = sm }

$(deriveSafeCopy 0 'base ''PlayerStore)

empty :: PlayerStore
empty = PlayerStore M.empty I.empty

newPlayer ::  Name -> PlayerStore -> PlayerStore
newPlayer name store = if validName (unName name) then 
                             case M.lookup name (nameMap store) of
                                 Nothing -> store'
                                 Just _ -> store
                       else store
                       where store' = PlayerStore names' scores'
                             p = StoredPlayer name 0 0
                             names' = M.insert name p $ nameMap store
                             scores' = addScore p $ scoreMap store

voteMod :: Name -> Int -> Int -> PlayerStore -> Maybe PlayerStore
voteMod _    0 0 store = Nothing
voteMod name u d store = fmap (voteModStored u d store) . M.lookup name . nameMap $ store

voteModStored :: Int -> Int -> PlayerStore -> StoredPlayer -> PlayerStore
voteModStored u d store stored = setNameMap names . setScoreMap scores $ store
    where names = M.insert name newPlayer . nameMap $ store
          scores = setOldLevel . setNewLevel . scoreMap $ store
          setOldLevel = if M.null oldScoreLevel
                            then I.delete oldScore 
                            else I.insert oldScore oldScoreLevel
          setNewLevel = I.insert newScore newScoreLevel
          oldScoreLevel = M.delete name . fromJust . I.lookup oldScore . scoreMap $ store
          newScoreLevel = M.insert name newPlayer . fromMaybe M.empty . I.lookup newScore . scoreMap $ store
          newPlayer = StoredPlayer name (playerUpvotes stored + u) (playerDownvotes stored + d)
          name = playerName stored
          oldScore = playerScore stored
          newScore = playerScore newPlayer
          

playerCount ::  PlayerStore -> Int
playerCount = M.size . nameMap

data RankStats = RankStats { rankName :: Name
                           , upvotes :: Int
                           , downvotes :: Int
                           , rank :: Int
                           }

getStats name store = fmap playerStats . M.lookup name . nameMap $ store

allStats store = map playerStats . concatMap (extractPlayers) . sortedResults $ store
    where sortedResults = reverse . I.toAscList . scoreMap 
          extractPlayers = map snd . M.toAscList . snd

searchStats x store = map playerStats . filter match . concatMap (extractPlayers) . sortedResults $ store
    where match = T.isInfixOf (normalize x) . normalize . unName . playerName
          sortedResults = reverse . I.toAscList . scoreMap 
          extractPlayers = map snd . M.toAscList . snd

playerStats p@(StoredPlayer n u d) store = RankStats n u d (fr . playerScore $ p) 
    where fr = findRank (scoreMap store)

findRank :: I.IntMap (M.Map k a) -> Int -> Int
findRank scores score  = 1 + I.foldr' sumSizes 0 higherRanked
    where higherRanked = snd $ I.split score scores
          sumSizes nameMap total = M.size nameMap + total

addScore :: StoredPlayer -> I.IntMap (M.Map Name StoredPlayer)
            -> I.IntMap (M.Map Name StoredPlayer)
addScore p scores = I.insert (playerScore p) nameMap scores
                    where nameMap = case I.lookup (playerScore p) scores of
                                        Nothing -> M.singleton (playerName p) p
                                        Just nm -> M.insert (playerName p) p nm

