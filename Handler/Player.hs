module Handler.Player
    ( getPlayerR
    , getPostR
    ) where

import Import
import Prelude (head, tail)

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
    (thread, meta) <- either500 $ query' acid $ GetThreadMeta num
    let name = case threadBoard meta of
                    PlayerBoard n -> n
                    _ -> ""
    player <- maybe404 $ query' acid $ PlayerStats name
    ip <- requestIP
    vote <- query' acid $ GetVote ip name
    render <- getUrlRender
    defaultLayout $ do
        $(widgetFile "comments")
        $(widgetFile "post")

statsHeader ::  Stats -> Vote -> GWidget App App ()
statsHeader player vote = $(widgetFile "statsheader")
