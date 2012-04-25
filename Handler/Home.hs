{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Function (on)
import Data.List (sortBy)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

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


doHome :: Maybe Player -> (Widget, Enctype) -> GHandler App App RepHtml
doHome newPlayer (formWidget, formEnctype) = do
    let submission = newPlayer
    acid <- getAcid
    players <- sortBy (flip compare `on` playerScore) <$> query' acid AllPlayers
    ip <- requestIP
    votes <- mapM (\x -> query' acid (GetVote ip (playerName x))) players
    let voteOf p = votes !! (fromJust $ elemIndex p players)
    let isUpvoted = (== Up) . voteOf
    let isDownvoted = (== Down) . voteOf
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
    _ <- case submission of
            Just p -> update' acid (SetPlayer p)
            Nothing -> return ()

    doHome submission (formWidget, formEnctype)


playerScore ::  Player -> Int
playerScore p = playerUpvotes p - playerDownvotes p `div` 2




getTableR :: Handler RepHtml
getTableR = do
    ip <- requestIP
    acid <- getAcid
    players <- sortBy (flip compare `on` playerScore) <$> query' acid AllPlayers
    votes <- mapM (\x -> query' acid (GetVote ip (playerName x))) players
    let voteOf p = votes !! (fromJust $ elemIndex p players)
    let isUpvoted = (== Up) . voteOf
    let isDownvoted = (== Down) . voteOf
    pc <- widgetToPageContent $(widgetFile "table")
    hamletToRepHtml [hamlet|
        ^{pageBody pc}
        |]
        

sampleForm :: Form Player
sampleForm = renderDivs $ Player
    <$> areq textField "Player name:" Nothing
    <*> areq intField "Upvotes" Nothing
    <*> areq intField "Downvotes" Nothing

