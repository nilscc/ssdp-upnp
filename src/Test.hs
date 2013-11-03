{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad

import Network
import Network.SSDP
import Network.UPnP

showDeviceInfo :: Upnp Device -> IO ()
showDeviceInfo dev = do

  putStrLn "\nDevice info:\n"

  putStrLn $ "\tLocation:      " ++ show (getUpnpURI dev)
  putStrLn $ "\tDevice type:   " ++ show (getDeviceType dev)
  putStrLn $ "\tFriendly name: " ++ getFriendlyName dev

  putStrLn "\n\tServices:\n"
  forM_ (getServiceList dev) $ \service -> do
    putStrLn $ "\t\tService type: " ++ show (getServiceType service)
    putStrLn $ "\t\tService ID:   " ++ show (getServiceId service)
    putStrLn $ "\t\tSCPDURL:      " ++ show (getSCPDURL service)
    putStrLn $ "\t\tControl URL:  " ++ show (getControlURL service)
    putStrLn $ "\t\tEventSubURL:  " ++ show (getEventSubURL service)
    putStrLn ""

  forM_ (getDeviceList dev) showDeviceInfo

showActionInfo :: Upnp Actions -> IO ()
showActionInfo actions = do

  putStrLn "\nActions:\n"

  mapM_ (putStrLn . ("\tName: " ++)) $ getActionNames actions

  putStrLn "\nGetConnectionTypeInfo:"

  let Just action = getActionByName "GetConnectionTypeInfo" actions
  forM_ (getArguments action) $ \adesc ->
    putStrLn $ "\t" ++ show adesc


discover :: IO ()
discover = do
  let ssdp = ssdpSearch (UrnDevice "schemas-upnp-org" "InternetGatewayDevice" "1")
                        Nothing Nothing
  (results,_) <- sendSearch ssdp
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
  (res,_) <- sendSearch ssdp
  let ((from, notify):_) = filter (hasHeader "LOCATION" . snd) res

  putStrLn $ "\nMsg [ " ++ show from ++ " ]:\n"
  putStrLn $ render notify

  -- get description
  mdev <- requestDeviceDescription notify
  case mdev of
    Right dev -> showDeviceInfo dev
    Left err  -> putStrLn $ "Error: " ++ err

findWANIPConnection1s :: IO ()
findWANIPConnection1s = do
  let ssdp = ssdpSearch UpnpRootDevice Nothing Nothing
  (results,_) <- sendSearch ssdp

  forM_ results $ \(from, notify) -> do

    dev <- requestDeviceDescription notify
    let wanip1 = standardService "WANIPConnection" "1"
    case dev of
      Right (findService wanip1 ->
             Just (getUpnpParent -> Just dev')) -> do
        putStrLn $ "Host [ " ++ show from ++ " ]"
        showDeviceInfo dev'
      _ -> return ()

getWANIPConnectionActions :: IO ()
getWANIPConnectionActions = do
  let ssdp = ssdpSearch (UrnService "schemas-upnp-org" "WANIPConnection" "1")
                        Nothing Nothing
  (((from, notify):_),_) <- sendSearch ssdp

  putStrLn $ "\nMsg [ " ++ show from ++ " ]:\n"
  putStrLn $ render notify

  Right dev <- requestDeviceDescription notify
  let Just service = findService (standardService "WANIPConnection" "1") dev
  Right actions <- requestActions service
  showActionInfo actions
  
main :: IO ()
main = withSocketsDo $ do
  --discover
  --getDescOfFirstIGD
  findWANIPConnection1s
