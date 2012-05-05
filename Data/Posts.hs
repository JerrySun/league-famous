{-# LANGUAGE DeriveDataTypeable #-}
module Data.Posts
    ( PostContent (..)
    , PostStore (..)
    , empty
    , newTopPostNC
    , newReply
    , getThread
    , playerThreads
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
import Control.Monad (mfilter)

data PostContent = PostContent { poster :: Text
                               , message :: Text
                               , imageUrl :: Maybe Text
                               , postTime :: UTCTime
                               } deriving (Typeable)

$(deriveSafeCopy 0 'base ''PostContent)

data PostEntry = TLPost PostContent [Int] | CLPost PostContent Int
                    deriving (Typeable)

extractPost (TLPost p _) = p
extractPost (CLPost p _) = p

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
getThread :: Int -> PostStore -> Maybe Thread
getThread number store = byNumber store number >>= doEntry
    where doEntry (CLPost _ pnum) = getThread pnum store
          doEntry tl@(TLPost _ _) = Just $ makeThread store tl

playerThreads :: Name -> PostStore -> [Thread]
playerThreads name store = map makeThread . filter isTL . catMaybes . map byNumber store $ pnums
    where pnums = fromMaybe [] $ M.lookup name (nameMap store)


-- No checking whether the Name is already a player in the ranks.  Do that in
-- the State version before running this, wrap in Maybe.
newTopPostNC :: Name -> PostContent -> PostStore -> (PostStore, Int)
newTopPostNC name post store = (incIndex . setNameMap names . inject posts $ store, index)
    where posts = I.insert index entry $ essence store
          index = nextIndex store
          entry = TLPost post []
          names = addIndexToName index name $ nameMap store

-- Maybe add the reply if parNum is an existing top-level post
newReply :: Int -> PostContent -> PostStore -> Maybe (PostStore, Int)
newReply parNum post store = fmap (newReplyOnTL post store) parent
    where parent = byNumber store parNum >>= mfilter isTL

-- Doesn't check whether parent exists
newReplyOnTL :: PostContent -> PostStore -> PostEntry -> (PostStore, Int)
newReplyOnTL post store parent = incIndex . setNameMap names . inject posts $ store
    where name = postPlayer . extractPost $ parent
          parent' = addChild index parent
          posts = I.insert parNum parent' $ I.insert index entry $ essence store
          index = nextIndex store
          entry = CLPost post parNum
          names = addIndexToName index name $ nameMap store

------- New helpers
addChild index (TLPost p children) = TLPost p (index:children)
addChild _ _ = error "addChild called on non-TLPost" -- How bad is this?

isTL :: PostEntry -> Bool
isTL (TLPost _ _) = True
isTL (CLPost _ _) = False


addIndexToName :: Int -> Name -> M.Map Name [Int] -> M.Map Name [Int]
addIndexToName index name names = M.insert name (index : existing) $ names
    where existing = fromMaybe [] $ M.lookup name names

makeThread :: PostStore -> PostEntry -> Thread
makeThread store tl@(TLPost tlContent childNums) = undefined
makeThread _ _ = error "makeThread called on non-TLPost" -- :/

--oldthing = (number, post) : map (id &&& extractPost . fromJust . byNumber store) (reverse pnums)
