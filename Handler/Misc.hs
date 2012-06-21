module Handler.Misc where

import Import

getFaqR :: Handler RepHtml
getFaqR = defaultLayout $(widgetFile "faq")

getErrorR :: Handler RepHtml
getErrorR = defaultLayout $(widgetFile "error")

getUserLoginR :: Handler RepHtml
getUserLoginR = defaultLayout $(widgetFile "userlogin")