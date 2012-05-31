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
import Data.Maybe (isJust)
import Control.Applicative ((<$>))

$(deriveSafeCopy 0 'base ''ImageType)
$(deriveSafeCopy 0 'base ''Thumbnail)

-- | Represents the content of a post
data Post = Post { poster :: Text -- ^ Name used by the poster
                 , message :: Text -- ^ Text body of the post
                 , image :: Maybe Thumbnail -- ^ Optional image thumbnail reference
                 , postTime :: UTCTime -- ^ Time and date of the post
                 } deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''Post)

-- | Stores a real post or something else in the post hierarchy
data PostContainer = NormalPost Post | Deleted deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''PostContainer)

-- | Thread data structure containing top post and any number of child posts
data Thread = Thread PostContainer (I.IntMap PostContainer) deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''Thread)

-- | Possible boards to which threads can be posted
data BoardId = PlayerBoard Name | General | OffTopic deriving (Show, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''BoardId)

-- | The data structure that stores threads for one board
type BoardThreads = I.IntMap Thread

-- | A map from board identifiers to corresponding threads
type BoardMap = M.Map BoardId BoardThreads

-- | Indicate either a parent post number or that the post is itself top-level
data Parent = Parent Int | Self deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''Parent)

-- | Master data structure that stores posts, along with indices and other state
data PostStore = PostStore { boards :: BoardMap -- ^ Post hierarchy with boards at the top level
                           , parents :: I.IntMap Parent -- ^ Map from post indices to their parents' indices
                           , topLevels :: I.IntMap BoardId -- ^ Map from thread parents to boards
                           , nextIndex :: Int -- ^ Index that will be assigned to a new post
                           } deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''PostStore)

data PostStoreError =   BoardDoesNotExist
                      | MissingThread
                      | PostStoreError
                      | NoParent
                      | NoBoard
                      deriving (Show)

type CanError a = Either PostStoreError a

-- Setters for 'PostStore' records

setBoards ::  BoardMap -> PostStore -> PostStore
setBoards x ps = ps {boards = x}

modBoards ::  (BoardMap -> BoardMap) -> PostStore -> PostStore
modBoards f ps = ps {boards = f (boards ps)}

setParents ::  I.IntMap Parent -> PostStore -> PostStore
setParents x ps = ps {parents = x}

modParents ::  (I.IntMap Parent -> I.IntMap Parent) -> PostStore -> PostStore
modParents f ps = ps {parents = f (parents ps)}

setTopLevels ::  I.IntMap BoardId -> PostStore -> PostStore
setTopLevels x ps = ps {topLevels = x}

modTopLevels :: (I.IntMap BoardId -> I.IntMap BoardId) -> PostStore -> PostStore
modTopLevels f ps = ps {topLevels = f (topLevels ps)}

-- | Empty initial post store
empty :: PostStore
empty = PostStore M.empty I.empty I.empty 0


-- | Increment the index counter
incIndex ::  PostStore -> PostStore
incIndex nm = nm { nextIndex = succ (nextIndex nm) }

-- Basic operations

-- Boards

createBoard :: BoardId -> PostStore -> PostStore
createBoard b store = setBoards boards' $ store
    where boards' = M.insert b emptyBoard . boards $ store
          emptyBoard = I.empty

getBoard :: BoardId -> PostStore -> CanError BoardThreads
getBoard b = maybeError BoardDoesNotExist . M.lookup b . boards

-- Threads

threadSingle :: PostContainer -> Thread
threadSingle p = Thread p I.empty

threadReply :: Int -> PostContainer -> Thread -> Thread
threadReply i r (Thread p rs) = Thread p (I.insert i r rs)

newThread :: BoardId -> PostContainer -> PostStore -> CanError (PostStore, Int)
newThread b p store = do
        board <- I.insert i (threadSingle p) <$> getBoard b store
        return (change board store, i)
    where i = nextIndex store
          change board = incIndex
                         . modBoards (M.insert b board)
                         . modParents (I.insert i Self)
                         . modTopLevels (I.insert i b)

getThread :: Int -> PostStore -> CanError Thread
getThread num store  = do
    parNum <- findParentIndex num store
    boardId <- findBoardId parNum store
    board <- getBoard boardId store
    thread <- maybeError MissingThread . I.lookup num $ board
    return thread

    

-- Replies

newReply :: Int -> PostContainer -> PostStore -> CanError (PostStore, Int)
newReply parNum post store = do
        thread <- threadReply i post <$> getThread parNum store
        boardId <- findBoardId parNum store
        board <- I.insert parNum thread <$> getBoard boardId store
        return (change boardId board store, i)
    where i = nextIndex store
          change bId board = incIndex
                         . modBoards (M.insert bId board)
                         . modParents (I.insert i (Parent parNum))


-- Posts

findParent :: Int -> PostStore -> CanError Parent
findParent num = maybeError NoParent . I.lookup num . parents

findParentIndex ::  Int -> PostStore -> Either PostStoreError Int
findParentIndex num store = process <$> findParent num store
    where process par = case par of
                            Self -> num
                            Parent n -> n

findBoardId :: Int -> PostStore -> Either PostStoreError BoardId
findBoardId parNum = maybeError NoBoard . I.lookup parNum . topLevels
-- etc

maybeError ::  a -> Maybe b -> Either a b
maybeError e = maybe (Left e) Right
