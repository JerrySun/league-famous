module Handler.Player
    ( getPlayerR
    , getPostR
    ) where

import Import
import Text.Lucius (luciusFile)
import Data.Maybe (fromMaybe)

maybe404 action = action >>= maybe notFound return

getPlayerR :: Name -> Handler RepHtml
getPlayerR name  = do
    acid <- getAcid
    player <- maybe404 $ query' acid (GetPlayer name)
    ip <- requestIP
    vote <- query' acid $ GetVote ip name
    defaultLayout $ do
        $(widgetFile "player")
        toWidget $(luciusFile "templates/teemo.lucius")

getPostR :: Int -> Handler RepHtml
getPostR _ = do
    let player = Player "Seinfeld" 44 17 73
    let vote = Up
    defaultLayout $ do
        $(widgetFile "post")
        toWidget $(luciusFile "templates/player.lucius")
        toWidget $(luciusFile "templates/teemo.lucius")
