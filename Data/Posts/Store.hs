{-# LANGUAGE DeriveDataTypeable #-}
-- | This module defines a message board data structure for user-defined post
-- and board id types.
module Data.Posts.Store 
    ( PostStore (..)
    , PostIx
    , Board
    , BoardMap
    , Parent (..)
    , empty
    -- * Thread operations
    , newThread
    , newReply
    , getThread
    , getThreadMeta
    -- * Board operations
    , createBoard
    , createBoardIfMissing
    , getBoard
    -- * Post operations
    , modifyPost
    -- * Error handling
    , PostStoreError (..)
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy, SafeCopy (..), contain, safePut, safeGet)
import Control.Monad (liftM4)
import qualified Data.Map as M
import qualified Data.IntMap as I
import Control.Applicative ((<$>))

import Data.Posts.Thread

--type PostIx = Int

-- | The data structure that stores threads for one board
type Board post = I.IntMap (Thread post)

-- | A map from board identifiers to corresponding threads
type BoardMap bid post = M.Map bid (Board post)

-- | Indicate either a parent post number or that the post is itself top-level
data Parent = Parent PostIx | Self deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''Parent)

-- | Master data structure that stores posts, along with indices and other state
data PostStore bid post =
    PostStore { -- | Post hierarchy with boards at the top level
                boards :: BoardMap bid post 
                -- | Map from post indices to their parents' indices
              , parents :: I.IntMap Parent
                -- | Map from thread parents' indices to the ids of the board on which they exist
              , threadLocations :: I.IntMap bid 
                -- | Index that will be assigned to a new post
              , nextIndex :: PostIx 
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
setTopLevels x ps = ps {threadLocations = x}

modTopLevels :: (I.IntMap b -> I.IntMap b) -> PostStore b p -> PostStore b p
modTopLevels f ps = ps {threadLocations = f (threadLocations ps)}

-- | Empty initial post store
empty :: PostStore b p
empty = PostStore M.empty I.empty I.empty 0


-- | Increment the index counter
incIndex ::  PostStore b p -> PostStore b p
incIndex nm = nm { nextIndex = succ (nextIndex nm) }

-- Boards

createBoard :: Ord b => b -> PostStore b p -> PostStore b p
createBoard b store = setBoards boards' store
    where boards' = M.insert b emptyBoard . boards $ store
          emptyBoard = I.empty

getBoard :: Ord b => b -> PostStore b p -> CanError (Board p)
getBoard b = maybeError BoardDoesNotExist . M.lookup b . boards

modifyBoard :: Ord b => (Board p -> Board p) -> b -> PostStore b p -> CanError (PostStore b p, Board p)
modifyBoard f b store = do
    board <- fmap f . getBoard b $ store
    let store' = modBoards (M.insert b board) store
    return  (store', board)



createBoardIfMissing :: Ord b => b -> PostStore b p -> Maybe (PostStore b p)
createBoardIfMissing b store = if not . M.member b . boards $ store
                                   then Just . createBoard b $ store
                                   else Nothing

-- Threads

-- | Create a new thread in the specified board. Fails if the board does not exist.
newThread :: Ord b => b -> p -> PostStore b p -> CanError (PostStore b p, PostIx)
newThread b p store = do
        board <- I.insert i (threadSingle p) <$> getBoard b store
        return (change board store, i)
    where i = nextIndex store
          change board = incIndex
                         . modBoards (M.insert b board)
                         . modParents (I.insert i Self)
                         . modTopLevels (I.insert i b)

-- | Create a reply to the given parent post index, returning the reply's index
newReply :: Ord b => PostIx -> p -> PostStore b p -> CanError (PostStore b p, PostIx)
newReply parNum post store = do
        thread <- threadReply i post <$> getThread parNum store
        boardId <- findBoardId parNum store
        board <- I.insert parNum thread <$> getBoard boardId store
        return (change boardId board store, i)
    where i = nextIndex store
          change bId board = incIndex
                         . modBoards (M.insert bId board)
                         . modParents (I.insert i (Parent parNum))

-- | Retrieve the thread that contains the given post index (as a reply or
-- parent comment)
getThread :: Ord b => PostIx -> PostStore b p -> CanError (Thread p)
getThread num store = fst <$> getThreadMeta num store

-- | Retrieve the thread that contains the given post index, along with its
-- location metadata
getThreadMeta :: Ord b => PostIx -> PostStore b p -> CanError (Thread p, ThreadMeta b)
getThreadMeta num store  = do
    parNum <- findParentIndex num store
    boardId <- findBoardId parNum store
    board <- getBoard boardId store
    thread <- maybeError MissingThread . I.lookup num $ board
    return (thread, ThreadMeta boardId parNum)


-- Posts

modifyPost :: Ord b => PostIx -> (p -> p) -> PostStore b p -> CanError (PostStore b p)
modifyPost num f store = do
    parent <- findParent num store
    let parNum = case parent of
                    Self -> num
                    Parent n -> n
    boardId <- findBoardId parNum store
    board <- getBoard boardId store
    thread <- maybeError MissingThread . I.lookup parNum $ board
    let thread' = case parent of
                    Self -> modpar thread
                    Parent _ -> modchil num thread
    (store', _) <- modifyBoard (I.insert parNum thread') boardId $ store
    return store'
  where
    modpar (Thread p cs) = Thread (f p) cs
    modchil i (Thread p cs) = Thread p (I.adjust f i cs)


findParent :: PostIx -> PostStore b p -> CanError Parent
findParent num = maybeError NoParent . I.lookup num . parents

findParentIndex ::  PostIx -> PostStore b p -> Either PostStoreError PostIx
findParentIndex num store = process <$> findParent num store
    where process par = case par of
                            Self -> num
                            Parent n -> n

findBoardId :: PostIx -> PostStore b p -> CanError b
findBoardId parNum = maybeError NoBoard . I.lookup parNum . threadLocations
-- etc

maybeError ::  a -> Maybe b -> Either a b
maybeError e = maybe (Left e) Right

