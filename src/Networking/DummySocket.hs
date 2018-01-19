{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Networking.DummySocket where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import qualified Data.ByteString         as BS

import           Networking

data DummySocket
  = DummySocket
      (Chan BS.ByteString)
      (MVar (Maybe BS.ByteString))
      (Chan BS.ByteString)

data instance Server DummySocket = DummySocketServer DummySocket
data instance ServerHooks DummySocket = DummySocketServerHooks
data instance Connection DummySocket = DummySocketConnection DummySocket
data instance ConnectionInfo DummySocket = DummySocketConnectionInfo

instance ServerStack DummySocket where
  data ServerConfig DummySocket = DummySocketConfig DummySocket
  withServer (DummySocketConfig s) h = h (DummySocketServer s)
  serveOnce (DummySocketServer s) _ h = h (DummySocketConnection s) DummySocketConnectionInfo

instance StreamOriented DummySocket where
  sendChunk (DummySocketConnection s) = send s
  receiveChunk (DummySocketConnection s) = receive s

newPair :: IO (DummySocket, DummySocket)
newPair = do
  c1 <- newChan
  c2 <- newChan
  m1 <- newMVar (Just mempty)
  m2 <- newMVar (Just mempty)
  pure (DummySocket c1 m1 c2, DummySocket c2 m2 c1)

send :: DummySocket -> BS.ByteString -> IO Int
send (DummySocket _ _ c) bs = writeChan c bs >> pure (BS.length bs)

receive :: DummySocket -> Int -> IO BS.ByteString
receive _ 0 = pure mempty
receive (DummySocket c m _) i =
  modifyMVar m $ \case
    Nothing -> pure (Nothing, mempty) -- connection closed by peer
    Just bs
      | BS.null bs       -> readChan c >>= \bs'-> if BS.null bs'
          then pure (Nothing, mempty)
          else pure (Just (BS.drop i bs'), BS.take i bs')
      | BS.length bs > i -> pure (Just (BS.drop i bs), BS.take i bs)
      | otherwise        -> pure (Just mempty, bs)

close :: DummySocket -> IO ()
close (DummySocket _ _ c) = writeChan c mempty


