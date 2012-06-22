module Handler.Misc where

import Import

getFaqR :: Handler RepHtml
getFaqR = defaultLayout $(widgetFile "faq")

getUserLoginR :: Handler RepHtml
getUserLoginR = defaultLayout $(widgetFile "userlogin")
