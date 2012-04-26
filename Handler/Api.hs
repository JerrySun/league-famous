module Handler.Api
    ( postUpvoteR
    , postDownvoteR
    , postNovoteR
    , postNewPlayerR
    ) where

import Import

postUpvoteR :: Name -> Handler RepJson
postUpvoteR = postVote Up

postDownvoteR :: Name -> Handler RepJson
postDownvoteR = postVote Down

postNovoteR :: Name -> Handler RepJson
postNovoteR = postVote Neutral

postVote :: Vote -> Name -> Handler RepJson
postVote vote name = do 
    ip <- requestIP
    acid <- getAcid
    _ <- update' acid $ ProcessVote ip name vote
    jsonToRepJson ()

postNewPlayerR :: Name -> Handler RepJson
postNewPlayerR name = do 
    acid <- getAcid
    _ <- update' acid $ NewPlayer name
    jsonToRepJson ()
