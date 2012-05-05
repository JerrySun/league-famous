{-# LANGUAGE DeriveDataTypeable #-}
module Data.Posts
    ( PostContent (..)
    , PostStore (..)
    , empty
    , newTopPostNC
    , newReply
    , getThread
    , playerThreads
    -------
    , Thread (..)
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
import Data.Name (Name)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (mfilter)

data PostContent = PostContent { posterContent :: Text
                               , messageContent :: Text
                               , imageUrlContent :: Maybe Text
                               , postTimeContent :: UTCTime
                               } deriving (Typeable)

$(deriveSafeCopy 0 'base ''PostContent)

data PostEntry = TLPost PostContent [Int] Name | CLPost PostContent Int
                    deriving (Typeable)

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

data Thread = Thread { parent :: Post
                     , reverseChildren :: [Post]
                     }

children = reverse . reverseChildren

data Post = Post { postNum :: Int
                 , poster :: Text
                 , message :: Text
                 , imageUrl :: Maybe Text
                 , postTime :: UTCTime
                 } 

setContent content post = post { poster = posterContent content
                               , message = messageContent content
                               , imageUrl = imageUrlContent content
                               , postTime = postTimeContent content
                               }

emptyPost = Post undefined undefined undefined undefined undefined

numberContents num content = setContent content $ emptyPost {postNum = num}

threadLength = (+ 1) . length . reverseChildren

------------------------------------------------------------------------------
getThread :: Int -> PostStore -> Maybe Thread
getThread number store = byNumber store number >>= doEntry
    where doEntry (CLPost _ pnum) = getThread pnum store
          doEntry tl@(TLPost _ _ _) = Just $ makeThread store number tl

playerThreads :: Name -> PostStore -> [Thread]
playerThreads name store = map (uncurry (makeThread store)) . filter (isTL . snd) . map (id &&& (fromJust . byNumber store)) $ pnums
    where pnums = fromMaybe [] $ M.lookup name (nameMap store)


-- No checking whether the Name is already a player in the ranks.  Do that in
-- the State version before running this, wrap in Maybe.
newTopPostNC :: Name -> PostContent -> PostStore -> (PostStore, Int)
newTopPostNC name post store = (incIndex . setNameMap names . inject posts $ store, index)
    where posts = I.insert index entry $ essence store
          index = nextIndex store
          entry = TLPost post [] name
          names = addIndexToName index name $ nameMap store

-- Maybe add the reply if parNum is an existing top-level post
newReply :: Int -> PostContent -> PostStore -> Maybe (PostStore, Int)
newReply parNum post store = fmap (newReplyOnTL post store parNum) parent
    where parent = mfilter isTL $ byNumber store parNum 

-- Doesn't check whether parent exists
newReplyOnTL :: PostContent -> PostStore -> Int -> PostEntry -> (PostStore, Int)
newReplyOnTL post store parNum parent = (incIndex . setNameMap names . inject posts $ store, index)
    where player = (\(TLPost _ _ name) -> name) parent
          parent' = addChild index parent
          posts = I.insert parNum parent' . I.insert index entry . essence $ store
          index = nextIndex store
          entry = CLPost post parNum
          names = addIndexToName index player $ nameMap store

------- New helpers
addChild index (TLPost p children player) = TLPost p (index:children) player
addChild _ _ = error "addChild called on non-TLPost" -- How bad is this?

isTL :: PostEntry -> Bool
isTL (TLPost _ _ _) = True
isTL (CLPost _ _) = False


addIndexToName :: Int -> Name -> M.Map Name [Int] -> M.Map Name [Int]
addIndexToName index name names = M.insert name (index : existing) $ names
    where existing = fromMaybe [] $ M.lookup name names

makeThread :: PostStore -> Int -> PostEntry -> Thread
makeThread store num tl@(TLPost tlContent childNums _) = Thread par childs
    where par = numberContents num tlContent
          childContents = map (extractContent . fromJust . byNumber store) childNums
          childs = zipWith numberContents childNums childContents

makeThread _ _ _ = error "makeThread called on non-TLPost" -- :/

--oldthing = (number, post) : map (id &&& extractPost . fromJust . byNumber store) (reverse pnums)
