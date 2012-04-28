module Handler.Home where

import Import
import Data.Function (on)
import Data.List (sortBy)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Hamlet (hamletFile)

playerScore ::  Player -> Int
playerScore p = playerUpvotes p - playerDownvotes p 

percent :: Player -> Int
percent p = f u d
            where u = playerUpvotes p
                  d = playerDownvotes p
                  f 0 0 = 0
                  f _ 0 = 100
                  f _ _ = (u * 100) `div` (u + d)
                

getHomeR :: Handler RepHtml
getHomeR = do
    playerTable <- makeTable
    defaultLayout $ setTitle "HELLO TEEMO" >> $(widgetFile "teemo") 


makeTable :: Handler (HtmlUrl (Route App))
makeTable = do
    acid <- getAcid
    players <- take 25 <$> query' acid AllPlayers
    ip <- requestIP
    votes <- mapM (\x -> query' acid (GetVote ip (playerName x))) players
    let voteOf p = votes !! (fromJust $ elemIndex p players)
    return $(hamletFile "templates/table.hamlet")

getTableR :: Handler RepHtml
getTableR = do
    table <- makeTable
    hamletToRepHtml table

getPreviewR :: Handler RepHtml
getPreviewR = do
    (name:_) <- parseJsonParam_
    acid <- getAcid
    maybePlayer <- query' acid $ GetPlayer name
    case maybePlayer of
        Just player -> hamletToRepHtml $(hamletFile "templates/preview.hamlet")
        Nothing -> notFound


