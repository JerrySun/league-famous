module Handler.Home where

import Import
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, fromJust)
import Text.Hamlet (hamletFile)
import Data.Text (unpack) 
import Safe (readMay)


getHomeR :: Handler RepHtml
getHomeR = getHomePageR 1


getHomePageR :: Int -> Handler RepHtml
getHomePageR pn = do
    acid <- getAcid
    maxPageNum <- (+ 1) . (`div` 20) <$> query' acid PlayerCount
    let pageNum = min (abs pn) maxPageNum
    playerTable <- makeTable pageNum 20 Nothing
    let untilReset = "1 hour 3 minutes" :: Text
    urlRenderParams <- getUrlRenderParams
    let pageRoute n = urlRenderParams (HomePageR n) []
    defaultLayout $ setTitle "League Famous" >> $(widgetFile "homepage") 

makeTable :: Int -> Int -> Maybe Text -> Handler (HtmlUrl (Route App))
makeTable page n mbSearch = do
    acid <- getAcid
    allPlayers <- case mbSearch of
                    Nothing -> query' acid AllStats
                    Just s -> query' acid $ SearchStats s
    let players = take n . drop ((page - 1) * n) $ allPlayers
    ip <- requestIP
    votes <- mapM (query' acid . GetVote ip . statName) players
    let voteOf p = votes !! fromJust (elemIndex p players)
    return $(hamletFile "templates/table.hamlet")


getTableR :: Handler RepHtml
getTableR = do
    params <- reqGetParams <$> getRequest
    let pageNum = fromMaybe 1 $ lookup "page" params >>= readMay . unpack
        searchStr = lookup "search" params
    table <- makeTable pageNum 20 searchStr
    hamletToRepHtml table

getPreviewR :: Handler RepHtml
getPreviewR = do
    (name:_) <- parseJsonParam_
    acid <- getAcid
    player <- maybe404 $ query' acid $ PlayerStats name
    threads <- fmap (take 4) $ query' acid $ PlayerThreads name
    hamletToRepHtml $(hamletFile "templates/preview.hamlet")


