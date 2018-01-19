{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
module Networking.WebSocket where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy          as BSL
import           Data.Int
import qualified Data.Set                      as S
import           Data.Typeable
import qualified Data.X509                     as X509
import qualified Network.TLS                   as TLS
import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Stream     as WS
import qualified System.Socket                 as SS
import qualified System.Socket.Type.Stream     as SS

import           Networking

data WebSocket a

newtype WebSocketServerException
  = ConnectionRejected WS.RejectRequest
  deriving (Eq, Show, Typeable)

instance Exception WebSocketServerException

deriving instance Eq WS.RejectRequest
deriving instance Show WS.RejectRequest
deriving instance Typeable WS.RejectRequest

data instance Server (WebSocket a) = WebSocketServer
  { wsServerConfig                  :: ServerConfig (WebSocket a)
  , wsTransportServer               :: Server a
  }

data instance ServerHooks (WebSocket a) = WebSocketServerHooks
  { wsOnConnectionRequest           :: WS.RequestHead -> IO (Either WS.RejectRequest WS.AcceptRequest)
  , wsTransportHooks                :: ServerHooks a
  }

data instance Connection (WebSocket a) = WebSocketConnection
  { wsTransportConnection           :: Connection a
  , wsConnection                    :: WS.Connection
  }

data instance ConnectionInfo (WebSocket a) = WebSocketConnectionInfo
  { wsTransportConnectionInfo       :: ConnectionInfo a
  , wsRequestHead                   :: WS.RequestHead
  }

instance (ServerStack a, StreamOriented a) => ServerStack (WebSocket a) where
  data ServerConfig (WebSocket a) = WebSocketServerConfig
    { wsTransportConfig             :: ServerConfig a
    , wsConnectionOptions           :: WS.ConnectionOptions
    }
  withServer config handler =
    withServer (wsTransportConfig config) $ \server->
      handler (WebSocketServer config server)
  serveOnce server hooks handler = serveOnce (wsTransportServer server) (wsTransportHooks hooks) (serveWithWebSocket server hooks handler)
  serveForever server hooks handler = serveForever (wsTransportServer server) (wsTransportHooks hooks) (serveWithWebSocket server hooks handler)

serveWithWebSocket :: (StreamOriented a) => Server (WebSocket a) -> ServerHooks (WebSocket a) -> (Connection (WebSocket a) -> ConnectionInfo (WebSocket a) -> IO b) -> Connection a -> ConnectionInfo a -> IO b
serveWithWebSocket server hooks handler connection info = do
  let readSocket = (\bs-> if BS.null bs then Nothing else Just bs) <$> receiveChunk connection 4096
  let writeSocket Nothing   = pure ()
      writeSocket (Just bs) = void (sendChunk connection (BSL.toStrict bs))
  stream <- WS.makeStream readSocket writeSocket
  request <- WS.makePendingConnectionFromStream stream (wsConnectionOptions $ wsServerConfig server)
  wsOnConnectionRequest hooks (WS.pendingRequest request) >>= \case
    Left x -> WS.rejectRequestWith request x >> throwIO (ConnectionRejected x)
    Right x -> WS.acceptRequestWith request x >>= \acceptedConnection-> do
      x <- handler
        (WebSocketConnection connection acceptedConnection)
        (WebSocketConnectionInfo info $ WS.pendingRequest request)
      WS.sendClose acceptedConnection ("Thank you for flying Haskell." :: BS.ByteString)
      pure x

deriving instance Show (ConnectionInfo a) => Show (ConnectionInfo (WebSocket a))

instance (StreamOriented a) => StreamOriented (WebSocket a) where
  sendChunk        connection bs  = WS.sendBinaryData (wsConnection connection) bs  >> pure (BS.length bs)
  sendChunkLazy    connection lbs = WS.sendBinaryData (wsConnection connection) lbs >> pure (BSL.length lbs)
  receiveChunkLazy connection _   = WS.receiveData    (wsConnection connection)
