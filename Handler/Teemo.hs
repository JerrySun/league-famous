module Handler.Teemo where

import Import
import Data.Function (on)
import Data.List (sortBy)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

playerScore ::  Player -> Int
playerScore p = playerUpvotes p - playerDownvotes p 

percent :: Player -> Int
percent p = f u d
            where u = playerUpvotes p
                  d = playerDownvotes p
                  f 0 0 = 0
                  f _ 0 = 100
                  f _ _ = (u * 100) `div` (u + d)
                

getTeemoR :: Handler RepHtml
getTeemoR = do
    acid <- getAcid
    players <- sortBy (flip compare `on` playerScore) <$> query' acid AllPlayers
    let playersAndRank = players `zip` ([1..] :: [Int])
    ip <- requestIP
    votes <- mapM (\x -> query' acid (GetVote ip (playerName x))) players
    let voteOf p = votes !! (fromJust $ elemIndex p players)
    defaultLayout $ setTitle "HELLO TEEMO" >> $(widgetFile "teemo") 

