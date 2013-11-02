{-# LANGUAGE ViewPatterns #-}

module Network.UPnP
  ( -- * Device description
    requestDeviceDescription
  , Upnp, getParentDevice
    -- ** Devices
  , Device
  , getDeviceType, DeviceType (..)
  , getFriendlyName, getManufacturer, getModelName, getUDN
  , getDeviceList
    -- ** Services
  , Service
  , getServiceList
  , getServiceType, ServiceType (..)
  , getServiceId, ServiceId (..)
  , getSCPDURL, getControlURL, getEventSubURL
  , findService
    -- ** Other
  , getStringValue, getRequiredStringValue
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Monoid
import Text.XML.Light
import Network.HTTP

import Network.SSDP
import Network.UPnP.Types
import Network.UPnP.Parser

-- helper
pickChildren :: (Element -> Bool) -> [Element] -> [Element]
pickChildren test = concatMap (filterChildren test)

hasElName :: String -> Element -> Bool
hasElName n (elName -> (qName -> n')) = n == n'

infixr 9 ~>
(~>) :: (a -> b) -> (b -> c) -> a -> c
(~>) = flip (.)

--------------------------------------------------------------------------------
-- Device description

requestDeviceDescription :: SSDP Notify -> IO (Maybe (Upnp Device))
requestDeviceDescription ssdp = runMaybeT $ do
  loc <- require $ getHeaderValue "LOCATION" ssdp
  res <- liftIO $ simpleHTTP (getRequest loc)
  bdy <- rspBody <$> requireRight res
  let xml  = parseXML bdy
      els  = onlyElems xml
      devs = pickChildren (hasElName "device") $ els
  case devs of
    [dev] -> return $ UpnpXml Nothing dev
    _     -> do
      liftIO (print devs)
      fail "Unexpected number of <device> tags."
 where
  require = maybe (fail "Unexpected Nothing") return 
  requireRight = either (\_ -> fail "Unexpected Left") return

--------------------------------------------------------------------------------
-- Helper

-- | Find a optional string valued device property
getStringValue :: String -> Upnp a -> Maybe String
getStringValue s (UpnpXml _ dev) =
  case pickChildren (hasElName s) [dev] of
    [elContent -> [Text (cdData -> str)]] -> Just str
    _ -> Nothing

-- | Find a required string valued device property. Uses `fail` on error.
getRequiredStringValue :: String -> Upnp a -> String
getRequiredStringValue s =
    maybe (fail $ "Tag <" ++ s ++ "> or its value not found.") id
  . getStringValue s

--------------------------------------------------------------------------------
-- Device values

getDeviceType :: Upnp Device -> Maybe DeviceType
getDeviceType d =
  case fmap parseDeviceType $ getStringValue "deviceType" d of
    Just (Right dt) -> Just dt
    _               -> Nothing

getFriendlyName, getManufacturer, getModelName, getUDN
  :: Upnp Device -> String
getFriendlyName = getRequiredStringValue "friendlyName"
getManufacturer = getRequiredStringValue "manufacturer"
getModelName    = getRequiredStringValue "modelName"
getUDN          = getRequiredStringValue "UDN"

getDeviceList :: Upnp Device -> [Upnp Device]
getDeviceList upnp@(UpnpXml _ dev) = map (UpnpXml (Just upnp)) $
     pickChildren (hasElName "deviceList")
  ~> pickChildren (hasElName "device")
   $ [dev]

--------------------------------------------------------------------------------
-- Services

getServiceList :: Upnp Device -> [Upnp Service]
getServiceList upnp@(UpnpXml _ dev) = map (UpnpXml (Just upnp)) $
     pickChildren (hasElName "serviceList")
  ~> pickChildren (hasElName "service")
   $ [dev]

getServiceType :: Upnp Service -> Maybe ServiceType
getServiceType d =
  case fmap parseServiceType $ getStringValue "serviceType" d of
    Just (Right st) -> Just st
    _               -> Nothing

getServiceId :: Upnp Service -> Maybe ServiceId
getServiceId d =
  case fmap parseServiceId $ getStringValue "serviceId" d of
    Just (Right sid) -> Just sid
    _                -> Nothing

getSCPDURL, getControlURL, getEventSubURL
  :: Upnp Service -> String
getSCPDURL     = getRequiredStringValue "SCPDURL"
getControlURL  = getRequiredStringValue "controlURL"
getEventSubURL = getRequiredStringValue "eventSubURL"

-- | Find a given service on a device or any of its embedded devices
findService :: ServiceType -> Upnp Device -> Maybe (Upnp Service)
findService sty dev =
  let services = getServiceList dev
      currentDev = listToMaybe $
        filter ((Just sty ==) . getServiceType) services
      embeddedDevs = map (findService sty) (getDeviceList dev)
   in getFirst' ( currentDev : embeddedDevs )
 where
  getFirst' = getFirst . mconcat . map First
