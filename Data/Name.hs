{-# LANGUAGE DeriveDataTypeable #-}
module Data.Name
    ( Name (..)
    , validName
    ) where

import Data.Text (Text)
import qualified Data.Text as T 
import qualified Data.Char as C

newtype Name = Name { unName :: Text }

$(deriveSafeCopy 0 'base ''Name)

normalize ::  Text -> Text
normalize = T.toLower . T.filter (not . C.isSpace)

instance Eq Name where
    Name x == Name y = normalize x == normalize y

instance Ord Name where
    Name x <= Name y = normalize x <= normalize y

instance IsString Name where
    fromString = Name . T.pack

instance FromJSON Name where
    parseJSON = fmap Name . parseJSON

instance ToHtml Name where
    toHtml = toHtml . unName
    
instance Show Name where
    show = show . unName

instance Read Name where
    readsPrec p str = [(Name . T.pack $ x,y) | (x,y) <- readsPrec p str]

instance PathPiece Name where
    fromPathPiece = fmap Name . fromPathPiece
    toPathPiece = unName


validName :: Text -> Bool
validName = (<= 16) . T.length
            &&* (>= 3) . T.length . T.filter (/= ' ')
            &&* T.all (C.isAlphaNum ||* (== ' '))


infixr 3 &&*
(&&*) ::  (t -> Bool) -> (t -> Bool) -> t -> Bool
(&&*) g h x = g x && h x

infixr 3 ||*
(||*) ::  (t -> Bool) -> (t -> Bool) -> t -> Bool
(||*) g h x = g x || h x
