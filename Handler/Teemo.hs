module Handler.Teemo where


import Import

getTeemoR :: Handler RepHtml
getTeemoR  = defaultLayout $(widgetFile "teemo")

