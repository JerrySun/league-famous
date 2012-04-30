module Helpers
    ( requestIP
    , parseJsonParam
    , parseJsonParam_
    , maybe404
    ) where

import Prelude
import Network.Wai (remoteHost, queryString)
import Network.Socket (SockAddr(..))
import Yesod.Handler (notFound, lift, getRequest, GHandler, invalidArgs, waiRequest)
import Yesod.Request (reqWaiRequest)
import Data.Text (pack)
import State (IP(..))
import qualified Data.Aeson as J
import Data.Attoparsec.ByteString (parse, maybeResult)

maybe404 action = action >>= maybe notFound return

requestIP = fmap (sockIP . remoteHost . reqWaiRequest) getRequest
            where sockIP (SockAddrInet _ a) = IPv4 a
                  sockIP (SockAddrInet6 _ _ a _) = IPv6 a
                  sockIP _ = IPv4 0 -- Kind of just don't handle unix sockets


parseJsonParam :: J.FromJSON a => GHandler sub master (J.Result a)
parseJsonParam = do
    params <- fmap queryString waiRequest

    case params of 
        [(bs, _)] -> case maybeResult $ parse (fmap J.fromJSON J.json') bs of
                        Just res -> return res
                        Nothing -> invalidArgs []
        _ -> invalidArgs []
    

-- | Same as 'parseJsonBody', but return an invalid args response on a parse
-- error.
parseJsonParam_ :: J.FromJSON a => GHandler sub master a
parseJsonParam_ = do
    ra <- parseJsonParam
    case ra of
        J.Error s -> invalidArgs [pack s]
        J.Success a -> return a
