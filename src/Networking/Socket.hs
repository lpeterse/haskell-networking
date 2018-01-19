{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
module Networking.Socket where

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
import qualified System.Socket                 as S
import qualified System.Socket.Type.Stream     as S

import           Networking

data instance Client (S.Socket f t p) = ClientSocket
  { csSocket :: !(S.Socket f t p)
  , csConfig :: !(ClientConfig (S.Socket f t p))
  }

data instance Server (S.Socket f t p) = SocketServer
  { socketServer       :: !(S.Socket f t p)
  , socketServerConfig :: !(ServerConfig (S.Socket f t p))
  }

data instance ServerHooks (S.Socket f t p) = SocketServerHooks

data instance Connection (S.Socket f t p) = SocketConnection !(S.Socket f t p)

data instance ConnectionInfo (S.Socket f t p) = SocketConnectionInfo !(S.SocketAddress f)

instance (S.Family f, S.Type t, S.Protocol p) => ClientStack (S.Socket f t p) where
  data ClientConfig (S.Socket f t p) = ClientSocketConfig
    { csDestinationAddress :: !(S.SocketAddress f)
    , csConnectionLimit    :: !Int
    }
  withClient = undefined
  withConnection = undefined

instance (S.Family f, S.Type t, S.Protocol p, Typeable f, Typeable t, Typeable p) => ServerStack (S.Socket f t p) where
  data ServerConfig (S.Socket f t p) = SocketServerConfig
    { socketServerConfigBindAddress     :: !(S.SocketAddress f)
    , socketServerConfigListenQueueSize :: !Int
    , socketServerConfigConnectionLimit :: !Int
    }
  withServer c handle = bracket
    (SocketServer <$> S.socket <*> pure c)
    (S.close . socketServer) $ \server-> do
      S.setSocketOption (socketServer server) (S.ReuseAddress True)
      S.bind (socketServer server) (socketServerConfigBindAddress $ socketServerConfig server)
      S.listen (socketServer server) (socketServerConfigListenQueueSize $ socketServerConfig server)
      handle server
  serveOnce server hooks handler = bracket
    ( S.accept $ socketServer server )
    ( S.close . fst )
    ( \(conn,addr)-> handler (SocketConnection conn) (SocketConnectionInfo addr) )
  serveForever server hooks handler = acquireAndHandleWithLimitedNumberOfThreads
    ( socketServerConfigConnectionLimit $ socketServerConfig server )
    ( bracket (S.accept $ socketServer server) (S.close . fst) )
    ( \(conn, addr)-> handler (SocketConnection conn) (SocketConnectionInfo addr) )

deriving instance Show (S.SocketAddress f) => Show (ConnectionInfo (S.Socket f t p))

instance (Typeable f, Typeable p, S.Family f, S.Protocol p) => StreamOriented (S.Socket f S.Stream p) where
  sendChunk (SocketConnection s) bs = S.sendAll s bs S.msgNoSignal
  sendChunkLazy (SocketConnection s) lbs = S.sendAllLazy s lbs S.msgNoSignal
  sendChunkBuilder (SocketConnection s) bufsize builder = S.sendAllBuilder s bufsize builder S.msgNoSignal
  receiveChunk (SocketConnection s) i = S.receive s i S.msgNoSignal

acquireAndHandleWithLimitedNumberOfThreads :: Int -> ((r -> IO ()) -> IO ()) -> (r -> IO ()) -> IO ()
acquireAndHandleWithLimitedNumberOfThreads maxThreads withResource handleResource = do
  qsem <- newQSem maxThreads
  withDependentFork $ \fork->
    forever $ do
      blocker <- newEmptyMVar
      fork $
        -- The child threads needs to acquire a unit of the quantity semaphore
        -- first. It eventually blocks until another thread terminates.
        -- The `bracket_` assures that the semaphore never leaks.
        bracket_ (waitQSem qsem) (signalQSem qsem) $
          -- Acquire the resource - eventually block here.
          -- `withResource` is responsible to safely release the resource after
          -- the handler function has returned or thrown.
          handle
            ( void . tryPutMVar blocker . Just :: SomeException -> IO () )
            $ withResource $ \resource-> do
              -- Signal the main thread that the child thread has acquired a
              -- resource.
              putMVar blocker Nothing
              -- Execute the resource handler within the child thread.
              handleResource resource
      -- The main thread waits here until the most recently forked child thread
      -- has acquired a resource. It then continues to fork off the next thread
      -- when `Nothing` has been signaled by the child thread or an exception
      -- is rethrown if the child thread encountered an exception during
      -- resource acquisition.
      takeMVar blocker >>= \case
        Nothing -> pure ()
        Just e  -> throwIO e

withDependentFork :: ((IO () -> IO ()) -> IO ()) -> IO ()
withDependentFork withFork = bracket before after within
  where
    within :: MVar (Maybe (S.Set ThreadId)) -> IO ()
    within  = withFork . forkWithRegistration

    before :: IO (MVar (Maybe (S.Set ThreadId)))
    before  = newMVar (Just mempty)

    after  :: MVar (Maybe (S.Set ThreadId)) -> IO ()
    after mtids = swapMVar mtids Nothing >>= \case
      Nothing -> pure ()
      Just tids -> mapM_ killThread tids

    forkWithRegistration :: MVar (Maybe (S.Set ThreadId)) -> IO () -> IO ()
    forkWithRegistration mtids action = void $ forkIO $ do
      tid <- myThreadId
      bracket_ (before' tid) (after' tid) action
      where
        before' tid =
          modifyMVar_ mtids $ \case
            Nothing   -> throwIO ThreadKilled -- We should have been killed.
            Just tids -> pure $! Just $! S.insert tid tids
        after' tid =
          modifyMVar_ mtids $ \case
            Nothing   -> pure Nothing -- Shouldn't happen, but doesn't do harm.
            Just tids -> pure $! Just $! S.delete tid tids
