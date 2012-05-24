module Helpers
    ( requestIP
    , parseJsonParam
    , parseJsonParam_
    , maybe404
    , niceTime
    , createThumbs
    ) where

import Prelude
import Network.Wai (remoteHost, queryString)
import Network.Socket (SockAddr(..))
import Yesod.Handler (notFound, getRequest, GHandler, invalidArgs, waiRequest)
import Yesod.Request (reqWaiRequest)
import Data.Text (Text, pack)
import State (IP(..))
import qualified Data.Aeson as J
import Data.Attoparsec.ByteString (parse, maybeResult)
import Data.Time (UTCTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (utcToLocalTime, TimeZone (..))
import System.Locale (defaultTimeLocale)
import Network.Thumbnail

maybe404 ::  GHandler sub master (Maybe b) -> GHandler sub master b
maybe404 action = action >>= maybe notFound return

requestIP ::  GHandler s m IP
requestIP = fmap (sockIP . remoteHost . reqWaiRequest) getRequest
            where sockIP (SockAddrInet _ a) = IPv4 a
                  sockIP (SockAddrInet6 _ _ a _) = IPv6 a
                  sockIP _ = IPv4 0 -- Kind of just don't handle unix sockets


-- FIXME this should parse the raw string, since not everything is actually
-- getting put in that first key of the parsed Query
parseJsonParam :: J.FromJSON a => GHandler sub master (J.Result a)
parseJsonParam = do
    params <- fmap queryString waiRequest

    case params of 
        [(bs, _)] -> case maybeResult $ parse (fmap J.fromJSON J.json') bs of
                        Just res -> return res
                        Nothing -> invalidArgs []
        _ -> invalidArgs []
    

parseJsonParam_ :: J.FromJSON a => GHandler sub master a
parseJsonParam_ = do
    ra <- parseJsonParam
    case ra of
        J.Error s -> invalidArgs [pack s]
        J.Success a -> return a

niceTime :: UTCTime -> String
niceTime = formatTime defaultTimeLocale "%D @ %l:%M %P PDT" . utcToLocalTime (TimeZone (-420) True "PDT")



thumbConfig :: ThumbConfig ThumbSize
thumbConfig = ThumbConfig { maxBytes = 10 * 1024 * 1024
                          , hashLength = 12
                          , saveRoot = "static/thumbs/"
                          , makeName = makeNameSplit 2
                          }

data ThumbSize = T100 | T80 deriving (Show, Enum, Bounded)

instance Dimensions ThumbSize where
    widthHeight T100 = (100, 100)
    widthHeight T80 = (80, 80)

    sizeTag = show

createThumbs :: Text -> IO (Either ThumbError Thumbnail)
createThumbs = thumbnail thumbConfig [minBound..maxBound]
