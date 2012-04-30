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
validName n = conditions n
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
                               , rankMap :: I.IntMap (M.Map Name Player) } deriving (Typeable)

$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''PlayerStore)

empty :: PlayerStore
empty = PlayerStore M.empty I.empty

voteMod :: Player -> Int -> Int -> PlayerStore -> PlayerStore
voteMod _ 0 0 store                      = store
voteMod p u d store@(PlayerStore names scores) = fromMaybe store newStore
    where pscore' = playerScore p + u - d
          newStore = fmap (\x -> PlayerStore (fst x) (snd x)) result
          result = do
              scores' <- do nameMap <- I.lookup (playerScore p) scores
                            let nameMap' = M.delete (playerName p) nameMap
                            return $ if M.null nameMap'
                               then I.delete (playerScore p) scores
                               else I.insert (playerScore p) nameMap' scores
              let rank = findRank (playerScore p) scores'
                  p' = (\(Player n up dn _ ) -> Player n (up + u) (dn + d) (-1)) p
                  scores'' = addScore p' scores'
                  names' = M.insert (playerName p') p' names
              return (names', scores'')

-- rank of a score not in the thing
findRank score scores = 1 + I.foldr' sumSizes 0 higherRanked
                        where higherRanked = snd $ I.split (score) scores
                              sumSizes nameMap total = M.size nameMap + total

addScore p scores = case I.lookup (playerScore p) scores of
                        Nothing -> I.insert (playerScore p) (M.singleton (playerName p) p) scores
                        Just nameMap -> I.insert (playerScore p) (M.insert (playerName p) p nameMap) scores




newPlayer name ranks = case validName (unName name) of 
                           True -> case M.lookup name (unwrapMap ranks) of
                                       Nothing -> ranks'
                                       Just _ -> ranks
                           False -> ranks
                       where ranks' = PlayerStore names' scores'
                             p = Player name 0 0 (-1)
                             names' = M.insert (playerName p) p $ unwrapMap ranks
                             scores' = addScore p $ rankMap ranks
                             
getPlayer n ranks = do
    player <- M.lookup n . unwrapMap $ ranks
    let pRank = findRank (playerScore player) (rankMap ranks)
    return $ player {playerRank = pRank}

refreshRank scores p = let pRank = findRank (playerScore p) scores
                       in p {playerRank = pRank}

allPlayers ranks = concatMap (map (refreshRank (rankMap ranks) . snd) . M.toAscList . snd) $ reverse $ I.toAscList (rankMap ranks)

searchPlayer x ranks = filter (T.isInfixOf (normalize x) . normalize . unName . playerName) $ allPlayers ranks
playerCount = M.size . unwrapMap
