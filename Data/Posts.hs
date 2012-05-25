{-# LANGUAGE DeriveDataTypeable #-}
module Data.Posts
    ( PostContent (..)
    , PostStore (..)
    , empty
    , newTopPostNC
    , newReply
    , getThread
    , playerThreads
    , commentCount
    , children
    , threadLength
    , recentChildren
    -------
    , Thread (..)
    , Post (..)
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, fromJust)
import Data.Time (UTCTime)
import Data.Name (Name)
import Control.Arrow ((&&&))
import Control.Monad (mfilter)
import qualified Data.ByteString as B
import Network.Thumbnail (Thumbnail (..), ImageType (..))

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

$(deriveSafeCopy 0 'base ''ImageType)
$(deriveSafeCopy 0 'base ''Thumbnail)

validatePostText :: Text -> Bool
validatePostText = (< 6000) . T.length 

data PostContent = PostContent { posterContent :: Text
                               , messageContent :: Text
                               , imageContent :: Maybe Thumbnail
                               , postTimeContent :: UTCTime
                               } deriving (Typeable)

$(deriveSafeCopy 0 'base ''PostContent)

data PostEntry = TLPost PostContent [Int] Name | CLPost PostContent Int
                    deriving (Typeable)

extractContent ::  PostEntry -> PostContent
extractContent (TLPost p _ _) = p
extractContent (CLPost p _) = p

$(deriveSafeCopy 0 'base ''PostEntry)

data PostStore = PostStore { essence :: I.IntMap PostEntry
                           , nameMap :: M.Map Name [Int]
                           , nextIndex :: Int
                           } deriving (Typeable)

$(deriveSafeCopy 0 'base ''PostStore)

inject :: I.IntMap PostEntry -> PostStore -> PostStore
inject im ps = ps {essence = im}

setNameMap ::  M.Map Name [Int] -> PostStore -> PostStore
setNameMap nm ps = ps {nameMap = nm}

incIndex ::  PostStore -> PostStore
incIndex nm = nm { nextIndex = succ (nextIndex nm) }

empty :: PostStore
empty = PostStore I.empty M.empty 0

byNumber ::  PostStore -> I.Key -> Maybe PostEntry
byNumber store number = I.lookup number . essence $ store

------------------------------------------------------------------------------
data Post = Post { postNum :: Int
                 , poster :: Text
                 , message :: Text
                 , image :: Maybe Thumbnail
                 , postTime :: UTCTime
                 } deriving (Typeable)

$(deriveSafeCopy 0 'base ''Post)

data Thread = Thread { parent :: Post
                     , reverseChildren :: [Post]
                     , threadPlayer :: Name
                     } deriving (Typeable)

$(deriveSafeCopy 0 'base ''Thread)

children ::  Thread -> [Post]
children = reverse . reverseChildren

threadLength ::  Thread -> Int
threadLength = (+ 1) . length . reverseChildren

setContent ::  PostContent -> Post -> Post
setContent content post = post { poster = posterContent content
                               , message = messageContent content
                               , image = imageContent content
                               , postTime = postTimeContent content
                               }

emptyPost ::  Post
emptyPost = Post undefined undefined undefined undefined undefined

numberContents ::  Int -> PostContent -> Post
numberContents num content = setContent content $ emptyPost {postNum = num}

recentChildren ::  Int -> Thread -> [Post]
recentChildren n = reverse . take n . reverseChildren

------------------------------------------------------------------------------
getThread :: Int -> PostStore -> Maybe Thread
getThread number store = byNumber store number >>= doEntry
    where doEntry (CLPost _ pnum) = getThread pnum store
          doEntry tl@(TLPost _ _ _) = Just $ makeThread store number tl

playerThreads :: Name -> PostStore -> [Thread]
playerThreads name store = map (uncurry (makeThread store)) . filter (isTL . snd) . map (id &&& (fromJust . byNumber store)) $ pnums
    where pnums = fromMaybe [] $ M.lookup name (nameMap store)

commentCount ::  Name -> PostStore -> Int
commentCount name store = fromMaybe 0 . fmap length . M.lookup name . nameMap $ store

-- No checking whether the Name is already a player in the ranks.  Do that in
-- the State version before running this, wrap in Maybe.
newTopPostNC :: Name -> PostContent -> PostStore -> Maybe (PostStore, Int)
newTopPostNC name post store = toMaybe (validatePostText . messageContent $ post)
                                       (incIndex . setNameMap names . inject posts $ store, index)
    where posts = I.insert index entry $ essence store
          index = nextIndex store
          entry = TLPost post [] name
          names = addIndexToName index name $ nameMap store

-- Maybe add the reply if parNum is an existing top-level post
newReply :: Int -> PostContent -> PostStore -> Maybe (PostStore, Int)
newReply parNum post store = do
    validPost <- mfilter (validatePostText . messageContent) $ return post
    parEntry <- mfilter isTL $ byNumber store parNum 
    return $ newReplyOnTL validPost store parNum parEntry

-- Doesn't check whether parent exists
newReplyOnTL :: PostContent -> PostStore -> Int -> PostEntry -> (PostStore, Int)
newReplyOnTL post store parNum parEntry = (incIndex . setNameMap names . inject posts $ store, index)
    where player = (\(TLPost _ _ name) -> name) parEntry
          parent' = addChild index parEntry
          posts = I.insert parNum parent' . I.insert index entry . essence $ store
          index = nextIndex store
          entry = CLPost post parNum
          names = addIndexToName index player $ nameMap store

------- New helpers
addChild ::  Int -> PostEntry -> PostEntry
addChild index (TLPost p childNums player) = TLPost p (index:childNums) player
addChild _ _ = error "addChild called on non-TLPost" -- How bad is this?

isTL :: PostEntry -> Bool
isTL TLPost{} = True
isTL CLPost{} = False


addIndexToName :: Int -> Name -> M.Map Name [Int] -> M.Map Name [Int]
addIndexToName index name names = M.insert name (index : existing) names
    where existing = fromMaybe [] $ M.lookup name names

makeThread :: PostStore -> Int -> PostEntry -> Thread
makeThread store num (TLPost tlContent childNums player) = Thread par childs player
    where par = numberContents num tlContent
          childContents = map (extractContent . fromJust . byNumber store) childNums
          childs = zipWith numberContents childNums childContents

makeThread _ _ _ = error "makeThread called on non-TLPost" -- :/
