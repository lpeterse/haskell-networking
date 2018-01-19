{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
module Networking.TLS where

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
import qualified System.Socket                 as SS
import qualified System.Socket.Type.Stream     as SS

import           Networking

data Tls a

data TlsServerException = TlsServerEndOfStreamException deriving (Eq, Typeable, Show)

instance Exception TlsServerException

data instance Server (Tls a) = TlsServer
  { tlsTransportServer            :: Server a
  , tlsServerConfig               :: ServerConfig (Tls a)
  }

data instance ServerHooks (Tls a) = TlsServerHooks
  { tlsServerHooks                :: TLS.ServerHooks
  , tlsTransportHooks             :: ServerHooks a
  }

data instance Connection (Tls a) = TlsConnection
  { tlsTransportConnection        :: Connection a
  , tlsContext                    :: TLS.Context
  }

data instance ConnectionInfo (Tls a) = TlsConnectionInfo
  { tlsTransportConnectionInfo    :: ConnectionInfo a
  , tlsCertificateChain           :: Maybe X509.CertificateChain
  }

instance (ServerStack a, StreamOriented a) => ServerStack (Tls a) where
  data ServerConfig (Tls a) = TlsServerConfig
    { tlsTransportConfig            :: ServerConfig a
    , tlsServerParams               :: TLS.ServerParams
    }
  withServer config handler =
    withServer (tlsTransportConfig config) $ \server->
      handler (TlsServer server config)
  serveOnce server hooks handler = serveOnce (tlsTransportServer server) (tlsTransportHooks hooks) (serveWithTls server hooks handler)
  serveForever server hooks handler = serveForever (tlsTransportServer server) (tlsTransportHooks hooks) (serveWithTls server hooks handler)

serveWithTls :: forall a b. (StreamOriented a)
  => Server (Tls a)
  -> ServerHooks (Tls a)
  -> (Connection (Tls a) -> ConnectionInfo (Tls a) -> IO b)
  -> Connection a
  -> ConnectionInfo a
  -> IO b
serveWithTls server hooks handler connection info = do
      let backend = TLS.Backend {
          TLS.backendFlush = pure () -- backend doesn't buffer
        , TLS.backendClose = pure () -- backend gets closed automatically
        , TLS.backendSend  = void . sendChunk connection
        -- The following is problematic: The Tls implementation requires us
        -- to return exactly as many bytes as requested. The underlying transport
        -- though only yields as many bytes as available.
        -- The solution is to read, append and loop until the request
        -- can be fulfilled.
        -- TODO: Use bytestring builder for concatenation.
        -- TODO: Fix Tls library upstream. The interface is awkward for a
        -- networking lib.
        , TLS.backendRecv = flip (receiveExactly connection) mempty
        }
      mvar <- newEmptyMVar
      let srvParams = tlsServerParams $ tlsServerConfig server
          srvParams' = srvParams {
            TLS.serverHooks = (TLS.serverHooks srvParams) {
              TLS.onClientCertificate = \certChain-> do
                putMVar mvar certChain
                pure TLS.CertificateUsageAccept
            }
          }
      context <- TLS.contextNew backend srvParams'
      TLS.handshake context
      certificateChain <- tryTakeMVar mvar
      x <- handler
        (TlsConnection connection context)
        (TlsConnectionInfo info certificateChain)
      TLS.bye context
      pure x
    where
      receiveExactly conn bytes accum = do
        bs <- receiveChunk conn bytes
        -- TCP sockets signal a graceful end of the stream by returning zero bytes.
        -- This function is not allowed to return less than the bytes
        -- request and we shall not loop forever here (we did!). There is no
        -- other option than throwing an exception here.
        when (BS.null bs) $ throwIO (TlsServerEndOfStreamException :: TlsServerException)
        if BS.length bs < bytes
          then receiveExactly connection (bytes - BS.length bs) $! accum `mappend` bs
          else pure $! accum `mappend` bs

deriving instance Show (ConnectionInfo a) => Show (ConnectionInfo (Tls a))

instance (StreamOriented a) => StreamOriented (Tls a) where
  sendChunkLazy connection lbs = TLS.sendData (tlsContext connection) lbs >> pure (BSL.length lbs)
  receiveChunk  connection _   = TLS.recvData (tlsContext connection)
