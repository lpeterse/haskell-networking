{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ProxyTest (tests) where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Networking
import qualified Networking.DummySocket    as DummySocket
import           Networking.Proxy
import qualified System.Socket.Family.Inet as Inet

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Networking.Proxy"
  [ test001
  , test002
  , test003
  , test004
  ]

test001 :: TestTree
test001 = testCase "Expecting a valid header to parse successfully" $ do
  (clientSock, serverSock) <- DummySocket.newPair
  void $ DummySocket.send clientSock "PROXY TCP4 0.0.0.0 0.0.0.0 23 47\r\n"
  withServer (ProxyConfig [] $ DummySocket.DummySocketConfig serverSock) $ \server-> do
    future <- async $ serveOnce server hooks $ \_ info-> pure (proxyHeader info)
    assertEqual "" expected =<< wait future
  where
    hooks = ProxyServerHooks DummySocket.DummySocketServerHooks
    expected = TCP4
      (Inet.SocketAddressInet Inet.inetAny 23)
      (Inet.SocketAddressInet Inet.inetAny 47)

test002 :: TestTree
test002 = testCase "Expecting a ProxyException on syntax error" $ do
  (clientSock, serverSock) <- DummySocket.newPair
  void $ DummySocket.send clientSock " PROXY TCP4 0.0.0.0 0.0.0.0 23 47\r\n"
  withServer (ProxyConfig [] $ DummySocket.DummySocketConfig serverSock) $ \server-> do
    future <- async $ serveOnce server hooks $ \_ info-> pure (proxyHeader info)
    swallowProxyException $ wait future >> assertFailure "Expected ProxyException"
  where
    hooks = ProxyServerHooks DummySocket.DummySocketServerHooks
    swallowProxyException a =
     a `catch` (\ProxyException {}-> pure ())

test003 :: TestTree
test003 = testCase "Expecting a ProxyException when peer closed socket" $ do
  (clientSock, serverSock) <- DummySocket.newPair
  DummySocket.close clientSock
  withServer (ProxyConfig [] $ DummySocket.DummySocketConfig serverSock) $ \server-> do
    future <- async $ serveOnce server hooks $ \_ info-> pure (proxyHeader info)
    swallowProxyException $ wait future >> assertFailure "Expected ProxyException"
  where
    hooks = ProxyServerHooks DummySocket.DummySocketServerHooks
    swallowProxyException a =
      a `catch` (\ProxyException {}-> pure ())

test004 :: TestTree
test004 = testCase "Expecting a ProxyException when header is not sent at once" $ do
  (clientSock, serverSock) <- DummySocket.newPair
  void $ DummySocket.send clientSock "PROXY TCP4 0.0.0.0 "
  void $ DummySocket.send clientSock "0.0.0.0 23 47\r\n"
  withServer (ProxyConfig [] $ DummySocket.DummySocketConfig serverSock) $ \server-> do
    future <- async $ serveOnce server hooks $ \_ info-> pure (proxyHeader info)
    swallowProxyException $ wait future >> assertFailure "Expected ProxyException"
  where
    hooks = ProxyServerHooks DummySocket.DummySocketServerHooks
    swallowProxyException a =
      a `catch` (\ProxyException {}-> pure ())
