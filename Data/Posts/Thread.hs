{-# LANGUAGE DeriveDataTypeable #-}
module Data.Posts.Thread 
    ( Thread (..)
    , ThreadMeta (..)
    , threadSingle
    , threadReply
    , threadLength
    , children
    , reverseChildren
    , recentChildren
    , parent
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Name (Name)
import Network.Thumbnail (Thumbnail, ImageType)
import Data.Maybe (isJust)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import qualified Data.IntMap as I
import qualified Data.Foldable as FO
import Data.Monoid (mappend)


-- | Thread data structure containing top post and any number of child posts
data Thread post = Thread post (I.IntMap post) deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''Thread)

instance Functor Thread where
    fmap f (Thread par chil) = Thread (f par) (fmap f chil)

instance FO.Foldable Thread where
    foldMap f (Thread par chil) = f par `mappend` FO.foldMap f chil

data ThreadMeta bid = ThreadMeta { threadBoard ::  bid
                             , threadIndex :: Int
                             } deriving (Typeable)
$(deriveSafeCopy 0 'base ''ThreadMeta)

-- Rename
singletonThread = threadSingle

threadSingle :: p -> Thread p
threadSingle p = Thread p I.empty

threadReply :: Int -> p -> Thread p -> Thread p
threadReply i r (Thread p rs) = Thread p (I.insert i r rs)

threadLength (Thread _ rs) = 1 + I.size rs

-- high level stuff from previous api
children ::  Thread p -> [(I.Key, p)]
children (Thread _ rs) = I.toAscList rs

reverseChildren ::  Thread p -> [(I.Key, p)]
reverseChildren = reverse . children

recentChildren ::  Int -> Thread p -> [(I.Key, p)]
recentChildren i = reverse . take i . reverseChildren


parent ::  Thread p -> p
parent (Thread p _) = p

threadPlayer _ = "Foo"
