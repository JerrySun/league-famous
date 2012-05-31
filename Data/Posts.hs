{-# LANGUAGE DeriveDataTypeable #-}
module Data.Posts
    ( PostStore (..)
    , Post (..)
    , Thread (..)
    , BoardId (..)
    , PostStoreError (..)
    , empty
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Name (Name)
import Network.Thumbnail (Thumbnail, ImageType)

$(deriveSafeCopy 0 'base ''ImageType)
$(deriveSafeCopy 0 'base ''Thumbnail)

-- | Represents the content of a post
data Post = Post { poster :: Text -- ^ Name used by the poster
                 , message :: Text -- ^ Text body of the post
                 , image :: Maybe Thumbnail -- ^ Optional image thumbnail reference
                 , postTime :: UTCTime -- ^ Time and date of the post
                 } deriving (Typeable)
$(deriveSafeCopy 0 'base ''Post)

-- | Stores a real post or something else in the post hierarchy
data PostContainer = NormalPost Post | Deleted deriving (Typeable)
$(deriveSafeCopy 0 'base ''PostContainer)

-- | Thread data structure containing top post and any number of child posts
data Thread = Thread PostContainer (I.IntMap PostContainer) deriving (Typeable)
$(deriveSafeCopy 0 'base ''Thread)

-- | Possible boards to which threads can be posted
data BoardId = PlayerBoard Name | General | OffTopic deriving (Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''BoardId)

-- | The data structure that stores threads for one board
type BoardThreads = I.IntMap Thread

-- | A map from board identifiers to corresponding threads
type BoardMap = M.Map BoardId BoardThreads

-- | Indicate either a parent post number or that the post is itself top-level
data Parent = Parent Int | Self deriving (Typeable)
$(deriveSafeCopy 0 'base ''Parent)

-- | Master data structure that stores posts, along with indices and other state
data PostStore = PostStore { boards :: BoardMap -- ^ Post hierarchy with boards at the top level
                           , parents :: I.IntMap Parent -- ^ Map from post indices to their parents' indices
                           , topLevels :: I.IntMap BoardId -- ^ Map from thread parents to boards
                           , nextIndex :: Int -- ^ Index that will be assigned to a new post
                           } deriving (Typeable)
$(deriveSafeCopy 0 'base ''PostStore)

data PostStoreError = PostStoreError deriving (Show)

-- Setters for 'PostStore' records

setBoards ::  BoardMap -> PostStore -> PostStore
setBoards x ps = ps {boards = x}

setParents ::  I.IntMap Parent -> PostStore -> PostStore
setParents x ps = ps {parents = x}

setTopLevels ::  I.IntMap BoardId -> PostStore -> PostStore
setTopLevels x ps = ps {topLevels = x}

-- | Empty initial post store
empty :: PostStore
empty = PostStore M.empty I.empty I.empty 0


-- | Increment the index counter
incIndex ::  PostStore -> PostStore
incIndex nm = nm { nextIndex = succ (nextIndex nm) }

