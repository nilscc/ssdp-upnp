module Main where

import Control.Applicative
import Control.Monad

import Network
import Network.SSDP
import Network.UPnP
import Network.UPnP.Types

showDeviceInfo :: Upnp Device -> IO ()
showDeviceInfo dev = do

  putStrLn "\nDevice info:\n"
  putStrLn $ "\tDevice type:   " ++ show (getDeviceType dev)
  putStrLn $ "\tFriendly name: " ++ getFriendlyName dev

  putStrLn "\n\tServices:\n"
  forM_ (getServiceList dev) $ \service -> do
    putStrLn $ "\t\tService type: " ++ show (getServiceType service)
    putStrLn $ "\t\tService ID:   " ++ show (getServiceId service)
    putStrLn $ "\t\tSCPDURL:      " ++ getSCPDURL service
    putStrLn $ "\t\tControl URL:  " ++ getControlURL service
    putStrLn $ "\t\tEventSubURL:  " ++ getEventSubURL service
    putStrLn ""

  forM_ (getDeviceList dev) showDeviceInfo


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

getDescOfFirstIGD :: IO ()
getDescOfFirstIGD = do

  -- SSDP search
  res <- sendSearch ssdp
  print $ length res
  let ((from, notify):_) = filter (hasHeader "LOCATION" . snd) res

  putStrLn $ "\nMsg [ " ++ show from ++ " ]:\n"
  putStrLn $ render notify

  -- get description
  Just dev <- requestDeviceDescription notify
  showDeviceInfo dev

 where
  ssdp = ssdpSearch (UrnDevice "schemas-upnp-org" "InternetGatewayDevice" "1") Nothing Nothing

findWANIPConnection1s :: IO ()
findWANIPConnection1s = do
  let ssdp = ssdpSearch UpnpRootDevice Nothing Nothing
  results <- sendSearch ssdp

  forM_ results $ \(_from, notify) -> do
    dev <- requestDeviceDescription notify
    let wanip1 = StandardService "WANIPConnection" "1"
    case join $ findService wanip1 <$> dev of
      Nothing -> return ()
      Just _  -> maybe (error "") showDeviceInfo dev
  
main :: IO ()
main = withSocketsDo $ do
  --discover
  --getDescOfFirstIGD
  findWANIPConnection1s
