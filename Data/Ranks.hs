{-# LANGUAGE DeriveDataTypeable #-}
module Data.Ranks
    ( Player (..)
    , PlayerStore 
    , empty
    , newPlayer
    , getPlayer
    , allPlayers
    , voteMod
    , playerCount
    , searchPlayer
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Aeson (FromJSON (..))
import Text.Blaze (ToHtml (..))
import Yesod.Dispatch (PathPiece (..))
import Data.Name

--TODO: remove all the stored rank, abstract stats out (into State module?)

data StoredPlayer = StoredPlayer { playerName :: Name
                                 , playerUpvotes :: Int
                                 , playerDownvotes :: Int
                                 } deriving (Eq, Typeable)


data PlayerStore = PlayerStore { unwrapMap :: M.Map Name StoredPlayer
                               , rankMap :: I.IntMap (M.Map Name StoredPlayer) }
                               deriving (Typeable)

$(deriveSafeCopy 0 'base ''StoredPlayer)
$(deriveSafeCopy 0 'base ''PlayerStore)

empty :: PlayerStore
empty = PlayerStore M.empty I.empty

voteMod :: Player -> Int -> Int -> PlayerStore -> PlayerStore
voteMod _ 0 0 store                      = store
voteMod p u d store@(PlayerStore names scores) = fromMaybe store newStore
    where newStore = fmap (uncurry PlayerStore) result
          result = do
              scores' <- do nameMap <- I.lookup (playerScore p) scores
                            let nameMap' = M.delete (playerName p) nameMap
                            return $ if M.null nameMap'
                               then I.delete (playerScore p) scores
                               else I.insert (playerScore p) nameMap' scores
              let p' = (\(StoredPlayer n up dn) -> Player n (up + u) (dn + d)) p
                  scores'' = addScore p' scores'
                  names' = M.insert (playerName p') p' names
              return (names', scores'')

-- rank of a score not in the thing
findRank ::  I.Key -> I.IntMap (M.Map k a) -> Int
findRank score scores = 1 + I.foldr' sumSizes 0 higherRanked
                        where higherRanked = snd $ I.split score scores
                              sumSizes nameMap total = M.size nameMap + total

addScore :: StoredPlayer-> I.IntMap (M.Map Name StoredPlayer)
            -> I.IntMap (M.Map Name StoredPlayer)
addScore p scores = I.insert (playerScore p) nameMap scores
                    where nameMap = case I.lookup (playerScore p) scores of
                                        Nothing -> M.singleton (playerName p) p
                                        Just nm -> M.insert (playerName p) p nm




newPlayer ::  Name -> PlayerStore -> PlayerStore
newPlayer name ranks = if validName (unName name) then 
                           case M.lookup name (unwrapMap ranks) of
                               Nothing -> ranks'
                               Just _ -> ranks
                       else ranks
                       where ranks' = PlayerStore names' scores'
                             p = Player name 0 0 (-1)
                             names' = M.insert (playerName p) p $ unwrapMap ranks
                             scores' = addScore p $ rankMap ranks
                             
getPlayer ::  Name -> PlayerStore -> Maybe StoredPlayer
getPlayer n ranks = do
    player <- M.lookup n . unwrapMap $ ranks
    let pRank = findRank (playerScore player) (rankMap ranks)
    return $ player {playerRank = pRank}

refreshRank ::  I.IntMap (M.Map k a) -> StoredPlayer -> StoredPlayer
refreshRank scores p = let pRank = findRank (playerScore p) scores
                       in p {playerRank = pRank}

allPlayers ::  PlayerStore -> [StoredPlayer]
allPlayers ranks = concatMap extractPlayer sortedResults
                   where extractPlayer = map (refreshRank (rankMap ranks) . snd)
                                         . M.toAscList . snd
                         sortedResults = reverse . I.toAscList . rankMap $ ranks

searchPlayer ::  Text -> PlayerStore -> [StoredPlayer]
searchPlayer x ranks = filter match $ allPlayers ranks
                       where match = T.isInfixOf (normalize x) . normalize
                                     . unName . playerName

playerCount ::  PlayerStore -> Int
playerCount = M.size . unwrapMap

playerScore :: PlayerStats -> Int
playerScore p = playerUpvotes p - playerDownvotes p 
