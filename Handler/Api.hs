module Handler.Api
    ( postNewPlayerR
    , postVoteR
    ) where

import Import
import Data.Aeson
import Control.Monad (mzero)
import Data.Time (getCurrentTime)

data VoteReq = VoteReq Vote Name

instance FromJSON VoteReq where
    parseJSON (Object v) = VoteReq <$> v .: "vote" <*> v .: "name"
    parseJSON _          = mzero

postVoteR :: Handler RepJson
postVoteR = do 
    VoteReq vote name <- parseJsonBody_
    ip <- requestIP
    acid <- getAcid
    _ <- update' acid $ ProcessVote ip name vote
    jsonToRepJson ()

postNewPlayerR :: Handler RepJson
postNewPlayerR = do 
    (name:_) <- parseJsonBody_
    acid <- getAcid
    _ <- update' acid $ NewPlayer name
    jsonToRepJson ()

data PostInput = PostInput Text Text Text Text

instance FromJSON PostInput where
    parseJSON (Object v) = PostInput <$> v .: "player" <*> v .: "name" <*> v .: "url" <*> v .: "text"
    parseJSON _          = mzero


postMakePostR :: Handler RepJson
postMakePostR = do
    (PostInput player name text url) <- parseJsonBody_
    time <- liftIO getCurrentTime
    let post = Post name text url time
    acid <- getAcid
    _ <- update' acid $ NewTopPost (Name player) post
    jsonToRepJson ()
    
