{-# LANGUAGE DeriveDataTypeable #-}
module Data.Posts.Store 
    ( PostStore (..)
    , empty
    , getThread
    , getThreadMeta
    , newReply
    , PostStoreError (..)
    , getBoard
    , newThread
    ) where

import Prelude
import Data.Typeable (Typeable, Typeable2)
import Data.SafeCopy (base, deriveSafeCopy, SafeCopy (..), contain, safePut, safeGet)
import Control.Monad (liftM4)
import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.Maybe (isJust)
import Control.Applicative ((<$>))
import Control.Arrow (second)

import Data.Posts.Thread

type PostId = Int

-- | The data structure that stores threads for one board
type BoardThreads post = I.IntMap (Thread post)

-- | A map from board identifiers to corresponding threads
type BoardMap bid post = M.Map bid (BoardThreads post)

-- | Indicate either a parent post number or that the post is itself top-level
data Parent = Parent PostId | Self deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''Parent)

-- | Master data structure that stores posts, along with indices and other state
data PostStore bid post = PostStore { boards :: BoardMap bid post -- ^ Post hierarchy with boards at the top level
                           , parents :: I.IntMap Parent -- ^ Map from post indices to their parents' indices
                           , topLevels :: I.IntMap bid -- ^ Map from thread parents to boards
                           , nextIndex :: PostId -- ^ Index that will be assigned to a new post
                           } deriving (Show, Typeable)


instance (Ord bid, SafeCopy bid, SafeCopy post) => SafeCopy (PostStore bid post) where
    getCopy = contain $ liftM4 PostStore safeGet safeGet safeGet safeGet
    putCopy (PostStore a b c d) = contain $ safePut a >> safePut b >> safePut c >> safePut d

data PostStoreError = BoardDoesNotExist
                    | MissingThread
                    | PostStoreError
                    | NoParent
                    | NoBoard
                      deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''PostStoreError)

type CanError a = Either PostStoreError a

-- Setters for 'PostStore' records

setBoards ::  BoardMap b p -> PostStore b p -> PostStore b p
setBoards x ps = ps {boards = x}

modBoards ::  (BoardMap b p -> BoardMap b p) -> PostStore b p -> PostStore b p
modBoards f ps = ps {boards = f (boards ps)}

setParents ::  I.IntMap Parent -> PostStore b p -> PostStore b p
setParents x ps = ps {parents = x}

modParents ::  (I.IntMap Parent -> I.IntMap Parent) -> PostStore b p -> PostStore b p
modParents f ps = ps {parents = f (parents ps)}

setTopLevels ::  I.IntMap b -> PostStore b p -> PostStore b p
setTopLevels x ps = ps {topLevels = x}

modTopLevels :: (I.IntMap b -> I.IntMap b) -> PostStore b p -> PostStore b p
modTopLevels f ps = ps {topLevels = f (topLevels ps)}

-- | Empty initial post store
empty :: PostStore b p
empty = PostStore M.empty I.empty I.empty 0


-- | Increment the index counter
incIndex ::  PostStore b p -> PostStore b p
incIndex nm = nm { nextIndex = succ (nextIndex nm) }

-- Boards

createBoard :: Ord b => b -> PostStore b p -> PostStore b p
createBoard b store = setBoards boards' $ store
    where boards' = M.insert b emptyBoard . boards $ store
          emptyBoard = I.empty

getBoard :: Ord b => b -> PostStore b p -> CanError (BoardThreads p)
getBoard b = maybeError BoardDoesNotExist . M.lookup b . boards

-- Threads

newThread :: Ord b => b -> p -> PostStore b p -> CanError (PostStore b p, PostId)
newThread b p store = do
        board <- I.insert i (threadSingle p) <$> getBoard b store
        return (change board store, i)
    where i = nextIndex store
          change board = incIndex
                         . modBoards (M.insert b board)
                         . modParents (I.insert i Self)
                         . modTopLevels (I.insert i b)

getThread :: Ord b => PostId -> PostStore b p -> CanError (Thread p)
getThread num store = fst <$> getThreadMeta num store

getThreadMeta :: Ord b => PostId -> PostStore b p -> CanError (Thread p, ThreadMeta b)
getThreadMeta num store  = do
    parNum <- findParentIndex num store
    boardId <- findBoardId parNum store
    board <- getBoard boardId store
    thread <- maybeError MissingThread . I.lookup num $ board
    return (thread, ThreadMeta boardId parNum)

-- Replies
newReply :: Ord b => PostId -> p -> PostStore b p -> CanError (PostStore b p, PostId)
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

findParent :: PostId -> PostStore b p -> CanError Parent
findParent num = maybeError NoParent . I.lookup num . parents

findParentIndex ::  PostId -> PostStore b p -> Either PostStoreError PostId
findParentIndex num store = process <$> findParent num store
    where process par = case par of
                            Self -> num
                            Parent n -> n

findBoardId :: PostId -> PostStore b p -> CanError b
findBoardId parNum = maybeError NoBoard . I.lookup parNum . topLevels
-- etc

maybeError ::  a -> Maybe b -> Either a b
maybeError e = maybe (Left e) Right

