{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative
import Control.Monad

import Network
import Network.SSDP
import Network.UPnP

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
  let ssdp = ssdpSearch UpnpRootDevice Nothing Nothing
  results <- sendSearch ssdp
  forM_ results $ \(from, msg) -> do

    putStrLn $ "\nMsg [ " ++ show from ++ " ]:\n"
    putStrLn $ "Server:   " ++ show (getHeaderValue "SERVER" msg)
    putStrLn $ "Location: " ++ show (getHeaderValue "LOCATION" msg)
    putStrLn $ "\n" ++ render msg

getDescOfFirstIGD :: IO ()
getDescOfFirstIGD = do

  -- SSDP search
  let ssdp = ssdpSearch (UrnDevice "schemas-upnp-org" "InternetGatewayDevice" "1")
                        Nothing Nothing
  res <- sendSearch ssdp
  let ((from, notify):_) = filter (hasHeader "LOCATION" . snd) res

  putStrLn $ "\nMsg [ " ++ show from ++ " ]:\n"
  putStrLn $ render notify

  -- get description
  Just dev <- requestDeviceDescription notify
  showDeviceInfo dev

findWANIPConnection1s :: IO ()
findWANIPConnection1s = do
  let ssdp = ssdpSearch UpnpRootDevice Nothing Nothing
  results <- sendSearch ssdp

  forM_ results $ \(from, notify) -> do

    dev <- requestDeviceDescription notify
    let wanip1 = StandardService "WANIPConnection" "1"
    case join $ findService wanip1 <$> dev of
      Just (getParentDevice -> Just dev') -> do
        putStrLn $ "Host [ " ++ show from ++ " ]"
        showDeviceInfo dev'
      _ -> return ()
  
main :: IO ()
main = withSocketsDo $ do
  --discover
  --getDescOfFirstIGD
  findWANIPConnection1s
