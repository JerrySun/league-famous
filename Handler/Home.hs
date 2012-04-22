{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

data Player = Player { playerName :: Text
                     , playerUpvotes :: Int
                     , playerDownvotes :: Int }

playerRatio :: Player -> Float
playerRatio p = let up = playerUpvotes p
                    down = playerDownvotes p
                    fI = fromIntegral
                in case up `compare` down of
                    GT -> fI up / fI down
                    LT -> fI down / fI up * (-1)
                    EQ -> 1

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let handlerName = "getHomeR" :: Text
        submission = Nothing :: Maybe Player 
    let allPlayers = [Player "Only Person" 1 0]
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")


postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
                        FormSuccess res -> Just res
                        _ -> Nothing
    
    let allPlayers = [Player "Only Person" 1 0]

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form Player
sampleForm = renderDivs $ Player
    <$> areq textField "Player name:" Nothing
    <*> areq intField "Upvotes" Nothing
    <*> areq intField "Downvotes" Nothing
