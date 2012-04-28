module Handler.Api
    ( postNewPlayerR
    , postVoteR
    ) where

import Import
import Data.Aeson
import Control.Monad (mzero)

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
