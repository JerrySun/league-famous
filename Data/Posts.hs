{-# LANGUAGE DeriveDataTypeable #-}
module Data.Posts
    ( module X 
    , PostContainer (..)
    , Post (..)
    , BoardId (..)
    , commentCount
    , playerThreads
    , newTopPostNC
    , hideDeleted
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.IntMap as I
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Name (Name)
import Network.Thumbnail (Thumbnail, ImageType)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<$>))
import Control.Arrow (second)

import Data.Posts.Thread as X
import Data.Posts.Store as X

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

-- | Possible boards to which threads can be posted
data BoardId = PlayerBoard Name | General | OffTopic deriving (Show, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''BoardId)

-- Basic operations


-- Replies

-- high level stuff from previous api
commentCount ::  Name -> PostStore BoardId p -> Int
commentCount name store = 
    case result of 
        Right x -> x
        Left _ -> 0
    where result = I.foldr ((+) . threadLength) 0 <$> getBoard (PlayerBoard name) store

hideDeleted :: Thread PostContainer -> Maybe (Thread Post)
hideDeleted (Thread Deleted _) = Nothing
hideDeleted t = Just . fmap unwrapPost . hideReplies $ t
    where hideReplies (Thread p c) = Thread p (I.filter isNormalPost c)
          unwrapPost (NormalPost x) = x
          isNormalPost (NormalPost _) = True
          isNormalPost _ = False


playerThreads :: Name -> PostStore BoardId PostContainer -> [(Thread Post, ThreadMeta BoardId)]
playerThreads name store =
    case result of
        Right x -> x
        Left _ -> []
    where result = do
              board <- getBoard (PlayerBoard name) store
              return
                . reverse
                . map (second (ThreadMeta (PlayerBoard name)) . uncurry (flip (,)))
                . mapMaybe hideDelPair
                . I.toAscList
                $ board

hideDelPair ::  (t, Thread PostContainer) -> Maybe (t, Thread Post)
hideDelPair (m, t) = fmap (\x -> (m,x)) . hideDeleted $ t

newTopPostNC :: Name -> Post -> PostStore BoardId PostContainer -> Maybe (PostStore BoardId PostContainer, Int)
newTopPostNC name post store =
    case result of
        Right x -> Just x
        Left _ -> Nothing
    where result = newThread (PlayerBoard name) (NormalPost post) store
