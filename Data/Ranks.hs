{-# LANGUAGE DeriveDataTypeable #-}
module Data.Ranks
    ( Player (..)
    , Name
    , PlayerStore 
    , empty
    , newPlayer
    , getPlayer
    , allPlayers
    , validName
    , playerScore
    , findRank
    , voteMod
    , playerCount
    , searchPlayer
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.Text (Text)
import qualified Data.Text as T 
import qualified Data.Char as C
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Aeson (FromJSON (..))
import Text.Blaze (ToHtml (..))
import Yesod.Dispatch (PathPiece (..))

data Player = Player { playerName :: Name
                     , playerUpvotes :: Int
                     , playerDownvotes :: Int
                     , playerRank :: Int
                     } deriving (Eq, Typeable)

newtype Name = Name { unName :: Text }


normalize ::  Text -> Text
normalize = T.toLower . T.filter (not . C.isSpace)

instance Eq Name where
    Name x == Name y = normalize x == normalize y

instance Ord Name where
    Name x <= Name y = normalize x <= normalize y

instance IsString Name where
    fromString = Name . T.pack

instance FromJSON Name where
    parseJSON = fmap Name . parseJSON

instance ToHtml Name where
    toHtml = toHtml . unName
    
instance Show Name where
    show = show . unName

instance Read Name where
    readsPrec p str = [(Name . T.pack $ x,y) | (x,y) <- readsPrec p str]

instance PathPiece Name where
    fromPathPiece = fmap Name . fromPathPiece
    toPathPiece = unName

$(deriveSafeCopy 0 'base ''Name)

validName :: Text -> Bool
validName = conditions
              where conditions = (<= 16) . T.length
                                 &&* (>= 3) . T.length . T.filter (/= ' ')
                                 &&* T.all (C.isAlphaNum ||* (== ' '))

playerScore ::  Player -> Int
playerScore p = playerUpvotes p - playerDownvotes p 

infixr 3 &&*
(&&*) ::  (t -> Bool) -> (t -> Bool) -> t -> Bool
(&&*) g h x = g x && h x

infixr 3 ||*
(||*) ::  (t -> Bool) -> (t -> Bool) -> t -> Bool
(||*) g h x = g x || h x

data PlayerStore = PlayerStore { unwrapMap :: M.Map Name Player
                               , rankMap :: I.IntMap (M.Map Name Player) }
                               deriving (Typeable)

$(deriveSafeCopy 0 'base ''Player)
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
              let p' = (\(Player n up dn _ ) -> Player n (up + u) (dn + d) (-1)) p
                  scores'' = addScore p' scores'
                  names' = M.insert (playerName p') p' names
              return (names', scores'')

-- rank of a score not in the thing
findRank ::  I.Key -> I.IntMap (M.Map k a) -> Int
findRank score scores = 1 + I.foldr' sumSizes 0 higherRanked
                        where higherRanked = snd $ I.split score scores
                              sumSizes nameMap total = M.size nameMap + total

addScore :: Player-> I.IntMap (M.Map Name Player)
            -> I.IntMap (M.Map Name Player)
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
                             
getPlayer ::  Name -> PlayerStore -> Maybe Player
getPlayer n ranks = do
    player <- M.lookup n . unwrapMap $ ranks
    let pRank = findRank (playerScore player) (rankMap ranks)
    return $ player {playerRank = pRank}

refreshRank ::  I.IntMap (M.Map k a) -> Player -> Player
refreshRank scores p = let pRank = findRank (playerScore p) scores
                       in p {playerRank = pRank}

allPlayers ::  PlayerStore -> [Player]
allPlayers ranks = concatMap extractPlayer sortedResults
                   where extractPlayer = map (refreshRank (rankMap ranks) . snd)
                                         . M.toAscList . snd
                         sortedResults = reverse . I.toAscList . rankMap $ ranks

searchPlayer ::  Text -> PlayerStore -> [Player]
searchPlayer x ranks = filter match $ allPlayers ranks
                       where match = T.isInfixOf (normalize x) . normalize
                                     . unName . playerName

playerCount ::  PlayerStore -> Int
playerCount = M.size . unwrapMap
