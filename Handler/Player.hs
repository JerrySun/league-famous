module Handler.Player
    ( getPlayerR
    , getPostR
    ) where

import Import
import Text.Lucius (luciusFile)
import Data.Maybe (fromMaybe)

getPlayerR :: Name -> Handler RepHtml
getPlayerR name  = do
    acid <- getAcid
    player <- maybe404 $ query' acid (GetPlayer name)
    ip <- requestIP
    vote <- query' acid $ GetVote ip name
    defaultLayout $ do
        $(widgetFile "comments")
        $(widgetFile "player")

data Post = Post { posterName :: Text
                 , postTime   :: Text
                 , postText   :: Text
                 , postImage  :: Maybe Text }

getPostR :: Int -> Handler RepHtml
getPostR _ = do
    let player = Player "Seinfeld" 44 17 73
    let vote = Up
    render <- getUrlRender
    let parent = Post "scarra" "4/20/2012 @ 4:20 PM" jellies (Just $ render (StaticR jellyfish_jpg))
    let children = [ Post "HotShotGG" "4/20/2012 @ 4:20 PM"
                     "Dude, Jellyfishes are lame. I think they are about as dumb as potatos and can't even speak English."
                     Nothing
                   , Post "Elementz" "4/20/2012 @ 4:20 PM"
                     "Does anyone like these?  What are these?!"
                     (Just $ render (StaticR okapi_jpg))
                   , Post "scarra" "4/20/2012 @ 4:20 PM"
                     "It's a horse you dummy and they can't even swim :p"
                     Nothing
                   ]
    defaultLayout $ do
        $(widgetFile "comments")
        $(widgetFile "post")

jellies = "I feel like jellyfish are the best creatures in the universe. How about you guys? Sometimes they can poison you, but I find them to be hilarious overall.Jellyfish (also known as jellies or sea jellies or a stage of the life cycle of Medusozoa) are free-swimming members of the phylum Cnidaria. Medusa is another word for jellyfish, and refers to any free-swimming jellyfish life stages among animals in the phylum. Jellyfish have multiple morphologies that represent cnidarian classes including the Scyphozoa (over 200 species), Staurozoa (about 50 species), Cubozoa (about 20 species), and Hydrozoa (about 1000¿1500 species that make jellyfish and many more that do not)."
