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
import Data.Function (on)

data Player = Player { playerName :: Text
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

emptyStore = PlayerStore M.empty

$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''PlayerStore)

setPlayer :: Player -> Update PlayerStore ()
setPlayer p = gets (M.insert (playerName p) p . unwrapMap) >>= put . PlayerStore

getPlayer :: Name -> Query PlayerStore (Maybe Player)
getPlayer n = asks $ M.lookup n . unwrapMap

allPlayers :: Query PlayerStore [Player]
allPlayers = asks $ M.elems . unwrapMap

$(makeAcidic ''PlayerStore ['setPlayer, 'getPlayer, 'allPlayers])
