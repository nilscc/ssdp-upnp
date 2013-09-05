module Main where

import Network
import Network.SSDP

discover :: IO ()
discover = do
  _ <- sendSearch ssdp callback
  return ()
 where
  ssdp              = ssdpSearch UpnpRootDevice Nothing Nothing
  callback from msg =
    putStrLn $ "\nMsg [ " ++ show from ++ " ]:\n\n" ++ renderSSDP msg

main :: IO ()
main = withSocketsDo $ do
  discover
