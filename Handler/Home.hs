{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Function (on)
import Data.List (sortBy)
import Text.Hamlet (hamletFile)
import Text.Cassius (cassiusFile)
import Network.Wai (remoteHost)
import Network.Socket (SockAddr(..))

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.


getHomeR :: Handler RepHtml
getHomeR = do
    form <- generateFormPost sampleForm
    doHome Nothing form


doHome newPlayer (formWidget, formEnctype) = do
    let submission = newPlayer
    acid <- getAcid
    players <- sortBy (flip compare `on` playerScore) <$> query' acid AllPlayers
    --ip <- fmap (show . remoteHost . reqWaiRequest) getRequest
    ip <- fmap (show . (\(SockAddrInet _ a) -> a) . remoteHost . reqWaiRequest) getRequest
    defaultLayout $ do
        let table = $(widgetFile "table")
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")


postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let submission = case result of
                        FormSuccess res -> Just res
                        _ -> Nothing
    
    acid <- getAcid
    case submission of
        Just p -> update' acid (SetPlayer p)
        Nothing -> return ()

    doHome submission (formWidget, formEnctype)


playerScore p = playerUpvotes p - playerDownvotes p `div` 2

getUpvoteR :: Name -> Handler RepHtml
getUpvoteR name = do 
    acid <- getAcid
    updateRes <- update' acid $ Upvote name
    case updateRes of
        Nothing -> notFound
        Just _ -> redirect HomeR

getDownvoteR :: Name -> Handler RepHtml
getDownvoteR name = do 
    acid <- getAcid
    updateRes <- update' acid $ Downvote name
    case updateRes of
        Nothing -> notFound
        Just _ -> redirect HomeR

getTableR :: Handler RepHtml
getTableR = do
    acid <- getAcid
    players <- sortBy (flip compare `on` playerScore) <$> query' acid AllPlayers
    pc <- widgetToPageContent $(widgetFile "table")
    hamletToRepHtml [hamlet|
        ^{pageBody pc}
        |]
        

sampleForm :: Form Player
sampleForm = renderDivs $ Player
    <$> areq textField "Player name:" Nothing
    <*> areq intField "Upvotes" Nothing
    <*> areq intField "Downvotes" Nothing

