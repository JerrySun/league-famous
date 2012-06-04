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
    , PostIx
    ) where

import Prelude
import Data.Typeable (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.IntMap as I
import qualified Data.Foldable as FO
import Data.Monoid (mappend)

type PostIx = Int

-- | Thread data structure containing top post and any number of child posts
data Thread post = Thread post (I.IntMap post) deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''Thread)

instance Functor Thread where
    fmap f (Thread par chil) = Thread (f par) (fmap f chil)

instance FO.Foldable Thread where
    foldMap f (Thread par chil) = f par `mappend` FO.foldMap f chil

data ThreadMeta bid = ThreadMeta { threadBoard ::  bid
                             , threadIndex :: PostIx
                             } deriving (Typeable)
$(deriveSafeCopy 0 'base ''ThreadMeta)

threadSingle :: p -> Thread p
threadSingle p = Thread p I.empty

threadReply :: PostIx -> p -> Thread p -> Thread p
threadReply i r (Thread p rs) = Thread p (I.insert i r rs)

threadLength ::  Thread p -> Int
threadLength (Thread _ rs) = 1 + I.size rs

-- high level stuff from previous api
children ::  Thread p -> [(PostIx, p)]
children (Thread _ rs) = I.toAscList rs

reverseChildren ::  Thread p -> [(PostIx, p)]
reverseChildren = reverse . children

recentChildren ::  PostIx -> Thread p -> [(PostIx, p)]
recentChildren i = reverse . take i . reverseChildren


parent ::  Thread p -> p
parent (Thread p _) = p
