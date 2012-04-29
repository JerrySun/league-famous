module Handler.Player
    ( getPlayerR
    , getPostR
    ) where

import Import
import Text.Lucius (luciusFile)


getPlayerR :: Name -> Handler RepHtml
getPlayerR name  = defaultLayout $ do
    $(widgetFile "player")
    toWidget $(luciusFile "templates/teemo.lucius")

getPostR :: Int -> Handler RepHtml
getPostR _ = defaultLayout $ do
    $(widgetFile "post")
    toWidget $(luciusFile "templates/player.lucius")
    toWidget $(luciusFile "templates/teemo.lucius")
