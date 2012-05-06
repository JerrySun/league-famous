module Handler.Player
    ( getPlayerR
    , getPostR
    ) where

import Import
import Control.Arrow ((&&&))
import Prelude (head, tail)
import Data.Maybe (fromMaybe)

getPlayerR :: Name -> Handler RepHtml
getPlayerR name  = do
    acid <- getAcid
    player <- maybe404 $ query' acid $ PlayerStats name
    ip <- requestIP
    vote <- query' acid $ GetVote ip name
    threads <- query' acid $ PlayerThreads name
    
    defaultLayout $ do
        $(widgetFile "comments")
        $(widgetFile "player")

getPostR :: Int -> Handler RepHtml
getPostR num = do
    acid <- getAcid
    thread <- maybe404 $ query' acid $ GetThread num
    let name = threadPlayer thread
    player <- maybe404 $ query' acid $ PlayerStats name
    ip <- requestIP
    vote <- query' acid $ GetVote ip name
    render <- getUrlRender
    defaultLayout $ do
        $(widgetFile "comments")
        $(widgetFile "post")

statsHeader player vote = $(widgetFile "statsheader")
