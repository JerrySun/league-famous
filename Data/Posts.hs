{-# LANGUAGE DeriveDataTypeable #-}
module Data.Posts
    ( Post (..)
    , PostStore (..)
    , empty
    , newTopPost
    , getThread
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.Text (Text)
import Data.Maybe (fromMaybe, fromJust)
import Data.Aeson (FromJSON (..))
import Data.Time (UTCTime)
import Data.Ranks (Name)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

data Post = Post { poster :: Text
                 , message :: Text
                 , imageUrl :: Text
                 , postTime :: UTCTime
                 } deriving (Typeable)

$(deriveSafeCopy 0 'base ''Post)

data PostEntry = TLPost Post [Int] | CLPost Post Int
                    deriving (Typeable)

extractPost (TLPost p _) = p
extractPost (CLPost p _) = p

$(deriveSafeCopy 0 'base ''PostEntry)

data PostStore = PostStore { essence :: I.IntMap PostEntry
                           , nameMap :: M.Map Name [Int]
                           , nextIndex :: Int
                           } deriving (Typeable)

$(deriveSafeCopy 0 'base ''PostStore)

-- Can be a record update later if needed
inject :: I.IntMap PostEntry -> PostStore -> PostStore
inject im ps = ps {essence = im}

setNameMap ::  M.Map Name [Int] -> PostStore -> PostStore
setNameMap nm ps = ps {nameMap = nm}

incIndex ::  PostStore -> PostStore
incIndex nm = nm { nextIndex = succ (nextIndex nm) }

empty :: PostStore
empty = PostStore I.empty M.empty 0

newTopPost ::  Name -> Post -> PostStore -> PostStore
newTopPost name post store = incIndex . setNameMap names . inject posts $ store
    where posts = I.insert index entry $ essence store
          index = nextIndex store
          entry = TLPost post []

          names = M.insert name (index : newList) $ nameMap store
          newList = fromMaybe [] $ M.lookup name (nameMap store)

byNumber ::  PostStore -> I.Key -> Maybe PostEntry
byNumber store number = I.lookup number . essence $ store

getThread ::  I.Key -> PostStore -> Maybe [(Int, Post)]
getThread number store = parentThread <$> (I.lookup number . essence) store
     where parentThread (TLPost tpost pnums) = (number, tpost) : map (id &&& extractPost . fromJust . byNumber store) (reverse pnums)
           parentThread (CLPost _ pnum) = parentThread . fromJust . byNumber store $ pnum


