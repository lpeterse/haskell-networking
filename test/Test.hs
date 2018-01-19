import           Test.Tasty

import qualified ProxyTest
import qualified ThreadingTest

main :: IO ()
main = defaultMain $ testGroup "Networking"
  [ ThreadingTest.tests
  , ProxyTest.tests
  ]
