{-# LANGUAGE DeriveDataTypeable #-}
module Data.Posts
    ( Post (..)
    , PostStore (..)
    , empty
    , newTopPost
    , getThread
    , recentTopPosts 
    , replyCount
    , newReply
    , getPost
    , recentSummaries
    , numPlayerPosts 
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
                 , imageUrl :: Maybe Text
                 , postTime :: UTCTime
                 , postPlayer :: Name
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

newReply ::  Int -> Post -> PostStore -> PostStore
newReply parNum post store = incIndex . setNameMap names . inject posts $ store
    where name = postPlayer . extractPost $ parent
          parent = fromJust . byNumber store $ parNum
          parent' = (\(TLPost p children) -> TLPost p (index:children)) parent
          posts = I.insert parNum parent' $ I.insert index entry $ essence store
          index = nextIndex store
          entry = CLPost post parNum

          names = M.insert name (index : newList) $ nameMap store
          newList = fromMaybe [] $ M.lookup name (nameMap store)

byNumber ::  PostStore -> I.Key -> Maybe PostEntry
byNumber store number = I.lookup number . essence $ store

getThread ::  I.Key -> PostStore -> Maybe [(Int, Post)]
getThread number store = case byNumber store number of
                            Nothing -> Nothing
                            Just (TLPost post pnums) -> Just $ (number, post) : map (id &&& extractPost . fromJust . byNumber store) (reverse pnums)
                            Just (CLPost _ pnum) -> getThread pnum store


recentTopPosts name store = fmap (map infoTriple . filter (isTop . snd) . map numAndPost) namePosts
    where namePosts = M.lookup name . nameMap $ store
          infoTriple (n, x) = (n, extractPost x, replyCount n store)
          numAndPost n = (n, fromJust . byNumber store $ n)
          isTop (TLPost _ _) = True
          isTop _ = False


recentSummaries name store = fmap (map miniThread . filter (isTop . snd) . map numAndPost) namePosts
    where namePosts = M.lookup name . nameMap $ store
          miniThread (n, x) = (n, extractPost x) :  map (id &&& extractPost . fromJust . byNumber store) ((reverse . take 2 . \(TLPost _ nums) -> nums) x)
          numAndPost n = (n, fromJust . byNumber store $ n)
          isTop (TLPost _ _) = True
          isTop _ = False

children :: Int -> PostStore -> [(Int, Post)]
children num store = case byNumber store num of
                         Just (TLPost _ xs) -> map (id &&& extractPost . fromJust . byNumber store) $ reverse xs
                         _ -> []

replyCount :: Int -> PostStore -> Int
replyCount num store = case byNumber store num of
                         Just (TLPost _ xs) -> length xs
                         _ -> 0

getPost num store = fmap extractPost . byNumber store $ num

numPlayerPosts name store = fromMaybe 0 . fmap length . M.lookup name . nameMap $ store
