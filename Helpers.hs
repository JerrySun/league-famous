module Helpers
    ( requestIP
    ) where

import Prelude
import Network.Wai (remoteHost)
import Network.Socket (SockAddr(..))
import Yesod.Handler (getRequest)
import Yesod.Request (reqWaiRequest)
import State (IP(..))

requestIP = fmap (sockIP . remoteHost . reqWaiRequest) getRequest
            where sockIP (SockAddrInet _ a) = IPv4 a
                  sockIP (SockAddrInet6 _ _ a _) = IPv6 a
                  sockIP _ = IPv4 0 -- Kind of just don't handle unix sockets
