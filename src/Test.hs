module Main where

import Text.XML.Light

import Network
import Network.SSDP
import Network.UPnP
import Network.UPnP.Types

discover :: IO ()
discover = do
  results <- sendSearch ssdp
  mapM_ put results
 where
  ssdp = ssdpSearch UpnpRootDevice Nothing Nothing
  put (from, msg) = do
    putStrLn $ "\nMsg [ " ++ show from ++ " ]:\n"
    putStrLn $ "Server:   " ++ show (getHeaderValue "SERVER" msg)
    putStrLn $ "Location: " ++ show (getHeaderValue "LOCATION" msg)
    putStrLn $ "\n" ++ render msg

getDescOfFirst :: IO ()
getDescOfFirst = do

  -- SSDP search
  res <- sendSearch ssdp
  let ((from, notify):_) = filter (hasHeader "LOCATION" . snd) res

  putStrLn $ "\nMsg [ " ++ show from ++ " ]:\n"
  putStrLn $ "Server:   " ++ show (getHeaderValue "SERVER" notify)
  putStrLn $ "Location: " ++ show (getHeaderValue "LOCATION" notify)
  putStrLn $ "\n" ++ render notify

  -- get description
  Just (UpnpXml cont) <- requestDeviceDescription notify

  putStrLn "\nDevice description:\n"
  mapM_ (putStrLn . ppContent) cont
 where
  ssdp = ssdpSearch UpnpRootDevice Nothing Nothing

main :: IO ()
main = withSocketsDo $ do
  --discover
  getDescOfFirst
