module DocTest (
  test
  ) where
import Test.DocTest

test :: IO ()
test = doctest ["-iNetwork", "Network/CGI/Protocol.hs"]
