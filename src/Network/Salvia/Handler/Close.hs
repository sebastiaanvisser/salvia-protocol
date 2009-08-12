module Network.Salvia.Handler.Close {- doc ok -}
  ( hCloseConn
  , hKeepAlive
  ) where

import Control.Monad.State
import Data.Maybe
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Core.Aspects
import System.IO

-- | Run a handler once and close the connection afterwards.

hCloseConn :: (SocketM m, MonadIO m) => m a -> m ()
hCloseConn h = h >> sock >>= liftIO . hClose

{- |
Run a handler and keep the connection open for potential consecutive requests.
The connection will only be closed after a request finished and one or more of
the following criteria are met:

* There is no `contentLength` set in the response headers. When this is the
  case the connection cannot be kept alive.

* The client has set the `connection` header field to 'close'.

* The connection has already been closed, possible due to IO errors.

* The HTTP version is HTTP/1.0.
-}

hKeepAlive :: (MonadIO m, SendM m, SocketM m, RequestM m, ResponseM m) => m a -> m ()
hKeepAlive handler =
  do handler
     h      <- sock
     conn   <- request (getM connection)
     ver    <- request (getM version)
     len    <- response (getM contentLength)
     closed <- liftIO (hIsClosed h)
     if or [ closed
           , conn == Just "Close"
           , isNothing (len :: Maybe Integer)
           , ver == http10
           ]
       then liftIO (hClose h)
       else resetContext >> hKeepAlive handler

resetContext :: (SocketM m, SendM m, RequestM m, ResponseM m) => m ()
resetContext =
  do request  (put emptyRequest)
     response (put emptyResponse)
     emptyQueue

