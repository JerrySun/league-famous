module Handler.Api
    ( postNewPlayerR
    , postVoteR
    , postMakePostR
    , postMakeReplyR
    ) where

import Import
import Data.Aeson
import Control.Monad (mzero, when)
import Data.Time (getCurrentTime)
import Data.Maybe (isNothing)

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

data PostInput = PostInput Text Text (Maybe Text) Text

instance FromJSON PostInput where
    parseJSON (Object v) = PostInput <$> v .: "player" <*> v .: "name" <*> v .:? "url" <*> v .: "text"
    parseJSON _          = mzero


postMakePostR :: Handler RepJson
postMakePostR = do
    PostInput player name1 url text <- parseJsonBody_
    when (text == "" && isNothing url) notFound
    let name = if name1 == "" then "Anonymous" else name1
    time <- liftIO getCurrentTime
    acid <- getAcid
    _ <- update' acid $ NewTopPost (Name player) $ PostContent name text url time
    jsonToRepJson ()
    
data ReplyInput = ReplyInput Int Text (Maybe Text) Text

instance FromJSON ReplyInput where
    parseJSON (Object v) = ReplyInput <$> v .: "parent" <*> v .: "name" <*> v .:? "url" <*> v .: "text"
    parseJSON _          = mzero

postMakeReplyR :: Handler RepJson
postMakeReplyR = do
    ReplyInput parNum name1 url text <- parseJsonBody_
    when (text == "" && isNothing url) notFound
    let name = if name1 == "" then "Anonymous" else name1
    acid <- getAcid
    time <- liftIO getCurrentTime
    postNumber <- maybe404 $ update' acid $ NewReply parNum $ PostContent name text url time
    jsonToRepJson postNumber
