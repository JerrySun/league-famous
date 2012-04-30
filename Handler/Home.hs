module Handler.Home where

import Import
import Data.Function (on)
import Data.List (sortBy)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Hamlet (hamletFile)
import Data.Text (pack, unpack) 
import Safe (readMay)

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
getHomeR = getHomePageR 1

tablePage n = makeTable (20 * (n - 1)) 20

getHomePageR :: Int -> Handler RepHtml
getHomePageR pn = do
    acid <- getAcid
    maxPageNum <- (+ 1) . (`div` 20) <$> query' acid PlayerCount
    let pageNum = min (abs pn) maxPageNum
    playerTable <- tablePage pageNum
    let untilReset = "1 hour 3 minutes" :: Text
    urlRenderParams <- getUrlRenderParams
    let pageRoute n = urlRenderParams (HomePageR n) []
    defaultLayout $ setTitle "HELLO TEEMO" >> $(widgetFile "teemo") 

makeTable :: Int -> Int -> Handler (HtmlUrl (Route App))
makeTable first n = do
    acid <- getAcid
    players <- take n . drop (max 0 (first - 1)) <$> query' acid AllPlayers
    ip <- requestIP
    votes <- mapM (\x -> query' acid (GetVote ip (playerName x))) players
    let voteOf p = votes !! (fromJust $ elemIndex p players)
    return $(hamletFile "templates/table.hamlet")

getTableR :: Handler RepHtml
getTableR = do
    params <- reqGetParams <$> getRequest
    pageNum <- maybe404 . return $ lookup "page" params >>= readMay . unpack
    table <- tablePage pageNum
    hamletToRepHtml table

getPreviewR :: Handler RepHtml
getPreviewR = do
    (name:_) <- parseJsonParam_
    acid <- getAcid
    maybePlayer <- query' acid $ GetPlayer name
    case maybePlayer of
        Just player -> hamletToRepHtml $(hamletFile "templates/preview.hamlet")
        Nothing -> notFound


