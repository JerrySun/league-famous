module Handler.Player
    (getPlayerR) where

import Import
import Text.Lucius (luciusFile)


getPlayerR :: Handler RepHtml
getPlayerR = defaultLayout $ do
    $(widgetFile "player")
    toWidget $(luciusFile "templates/teemo.lucius")
