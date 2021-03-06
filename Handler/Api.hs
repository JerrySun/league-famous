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

data PostInput = PostInput { inputPlayer :: Text
                           , inputName :: Text
                           , inputUrl :: Maybe Text
                           , inputBody :: Text
                           }

instance FromJSON PostInput where
    parseJSON (Object v) = PostInput <$> v .: "player" <*> v .: "name" <*> v .:? "url" <*> v .: "text"
    parseJSON _          = mzero

maybeRight :: Either z a -> Maybe a
maybeRight x = case x of 
    Right y -> Just y
    Left _ -> Nothing

postMakePostR :: Handler RepJson
postMakePostR = do
    PostInput player name1 url text <- parseJsonBody_
    when (text == "" && isNothing url) notFound
    let name = if name1 == "" then "Anonymous" else name1
    thumb_ <- case url of
        Just u -> liftIO . createThumbs $ u
        Nothing -> return $ Left ThumbError
    let thumb = maybeRight thumb_
    time <- liftIO getCurrentTime
    acid <- getAcid
    num <- update' acid $ NewTopPost (Name player) $ Post name text thumb time
    jsonToRepJson num
    
data ReplyInput = ReplyInput { replyParent :: Int
                           , replyName :: Text
                           , replyUrl :: Maybe Text
                           , replyBody :: Text
                           }

instance FromJSON ReplyInput where
    parseJSON (Object v) = ReplyInput <$> v .: "parent" <*> v .: "name" <*> v .:? "url" <*> v .: "text"
    parseJSON _          = mzero

postMakeReplyR :: Handler RepJson
postMakeReplyR = do
    ReplyInput parNum name1 url text <- parseJsonBody_
    when (text == "" && isNothing url) notFound
    let name = if name1 == "" then "Anonymous" else name1
    thumb_ <- case url of
        Just u -> liftIO . createThumbs $ u
        Nothing -> return $ Left ThumbError
    let thumb = maybeRight thumb_
    acid <- getAcid
    time <- liftIO getCurrentTime
    postNumber <- either500 $ update' acid $ NewReply parNum $ Post name text thumb time
    jsonToRepJson postNumber
