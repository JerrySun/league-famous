module Helpers
    ( requestIP
    , parseJsonParam
    , parseJsonParam_
    , maybe404
    , either500
    , niceTime
    , createThumbs
    , Thumbnail (..)
    , ThumbError (..) , ThumbSize (..)
    , thumbR
    ) where

import Prelude
import Network.Wai (remoteHost, queryString, requestHeaders)
import Network.Socket (SockAddr(..))
import Yesod.Handler (notFound, getRequest, GHandler, invalidArgs, waiRequest)
import Yesod.Request (reqWaiRequest)
import qualified Data.Text as T
import qualified Data.Aeson as J
import Data.Attoparsec.ByteString (parse, maybeResult)
import Data.Time (UTCTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (utcToLocalTime, TimeZone (..))
import System.Locale (defaultTimeLocale)
import Network.Thumbnail
import Foundation (Route (..), App)
import Data.IP.Address (IP (..), toIP, toBits6)
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)

maybe404 ::  GHandler sub master (Maybe b) -> GHandler sub master b
maybe404 action = action >>= maybe notFound return

either500 ::  (Monad m, Show a) => m (Either a b) -> m b
either500 action = do
    result <- action
    case result of
        Right x -> return x
        Left y -> fail $ show y

-- Note: the address coming from remoteHost is little-endian
requestIP' ::  GHandler s m IP
requestIP' = fmap (sockIP . remoteHost . reqWaiRequest) getRequest
            where sockIP (SockAddrInet _ a) = IPv4 a
                  sockIP (SockAddrInet6 _ _ a _) = IPv6 . fromJust . toBits6 $ a
                  sockIP _ = IPv4 0 -- Kind of just don't handle unix sockets

requestIP :: GHandler sub master IP
requestIP = (>>= maybe (fail "no ip") return) . fmap realIP $ getRequest
    where realIP = (>>= toIP) . lookup "X-Real-IP" . requestHeaders . reqWaiRequest


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
        J.Error s -> invalidArgs [T.pack s]
        J.Success a -> return a

niceTime :: UTCTime -> String
niceTime = formatTime defaultTimeLocale "%D @ %l:%M %P PDT" . utcToLocalTime (TimeZone (-420) True "PDT")



thumbConfig :: ThumbConfig ThumbSize
thumbConfig = ThumbConfig { maxBytes = 10 * 1024 * 1024
                          , hashLength = 12
                          , saveRoot = "static/thumbs/"
                          , makeName = makeNameSplit 2
                          }

data ThumbSize = T100 | T120 | T200 deriving (Show, Enum, Bounded)

instance Dimensions ThumbSize where
    widthHeight T100 = (100, 100)
    widthHeight T120 = (120, 120)
    widthHeight T200 = (200, 200)

    sizeTag = show

createThumbs :: T.Text -> IO (Either ThumbError Thumbnail)
createThumbs = thumbnail thumbConfig [minBound..maxBound]

partitions :: Eq a => a -> [a] -> [[a]]
partitions _ [] = []
partitions a as = 
 case break (==a) as of
   (xs,[])   -> [xs]
   (xs,_:ys) -> xs:partitions a ys

thumbR :: ByteString -> ImageType -> ThumbSize -> Route App
thumbR hash imgt size = StaticR . simpleStaticRoute $ makeName thumbConfig imgt hash size
    where simpleStaticRoute fpath = StaticRoute (fmap T.pack . drop 1 . partitions '/' . (saveRoot thumbConfig ++) $ fpath) []
