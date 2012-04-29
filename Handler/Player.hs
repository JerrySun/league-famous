module Handler.Player
    ( getPlayerR
    , getPostR
    ) where

import Import
import Text.Lucius (luciusFile)


getPlayerR :: Handler RepHtml
getPlayerR = defaultLayout $ do
    $(widgetFile "player")
    toWidget $(luciusFile "templates/teemo.lucius")

getPostR :: Handler RepHtml
getPostR = defaultLayout $ do
    $(widgetFile "post")
    toWidget $(luciusFile "templates/player.lucius")
    toWidget $(luciusFile "templates/teemo.lucius")
