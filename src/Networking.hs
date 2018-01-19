{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Networking
-- Copyright   :  (c) Lars Petersen 2017
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module Networking where

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

data family Client a
data family Server a
data family ServerHooks a
data family Connection a
data family ConnectionInfo a

type ConnectionHandler a b = Connection a -> ConnectionInfo a -> IO b

class ClientStack a where
  data ClientConfig a
  withClient :: ClientConfig a -> IO (Client a)
  withConnection :: Client a -> ConnectionHandler a b -> IO b
  {-# MINIMAL withClient, withConnection #-}

class ServerStack a where
  data ServerConfig a
  -- | Creates a new server from a configuration and passes it to a handler function.
  --
  --   The server given to the handler function shall be bound and in
  --   listening state. The handler function is usually a
  --   `Control.Monad.forever` loop that accepts and handles new connections.
  --
  --   > withServer config $ \server->
  --   >   forever $ withConnection handleConnection
  withServer     :: ServerConfig a -> (Server a -> IO b) -> IO b
  serveOnce      :: Server a -> ServerHooks a -> ConnectionHandler a b -> IO b
  -- | Waits for and accepts new connections from a listening server and passes
  --   them to a handler function executed in a new thread.
  --
  --   This operation blocks until it receives an exception.
  --
  --   > withServer config $ \server-> serve server handleConnection
  --   >   where
  --   >     handleConnection conn info = do
  --   >       tid <- myThreadId
  --   >       putStrLn $ "Thread " ++ show tid ++ " is now serving connection " ++ show info ++ "."
  serveForever :: Server a -> ServerHooks a -> (Connection a -> ConnectionInfo a -> IO ()) -> IO ()
  serveForever server handler = forever $ void $ serveOnce server handler
  {-# MINIMAL withServer, serveOnce #-}

class StreamOriented a where
  sendChunk               :: Connection a -> BS.ByteString -> IO Int
  sendChunk server bs      = fromIntegral <$> sendChunkLazy server (BSL.fromStrict bs)
  sendChunkLazy           :: Connection a -> BSL.ByteString -> IO Int64
  sendChunkLazy server     = foldM
    (\sent bs-> sendChunk server bs >>= \sent'-> pure $! sent + fromIntegral sent') 0 . BSL.toChunks
  sendChunkBuilder        :: Connection a -> Int -> BS.Builder -> IO Int64
  sendChunkBuilder server chunksize = sendChunkLazy server
    . BS.toLazyByteStringWith (BS.untrimmedStrategy chunksize chunksize) mempty
  receiveChunk            :: Connection a -> Int -> IO BS.ByteString
  receiveChunk server i    = BSL.toStrict <$> receiveChunkLazy server i
  receiveChunkLazy        :: Connection a -> Int -> IO BSL.ByteString
  receiveChunkLazy server i = BSL.fromStrict <$> receiveChunk server i
  {-# MINIMAL (sendChunk|sendChunkLazy), (receiveChunk|receiveChunkLazy) #-}
