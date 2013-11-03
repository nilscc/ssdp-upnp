{-# LANGUAGE EmptyDataDecls #-}

module Network.UPnP.Types where

import Network.URI
import Text.XML.Light

data Upnp a
  = UpnpXml { getParentDevice   :: Maybe (Upnp Device)
            , getUpnpURI        :: URI
            , getUpnpXmlContent :: Element
            }
  deriving (Show)

data Device
data Service

data DeviceType
  = DeviceType { deviceVendorDomain :: String
               , deviceType         :: String
               , deviceVersion      :: String
               }
  deriving (Show, Eq)

data ServiceType
  = ServiceType { serviceVendorDomain :: String
                , serviceType         :: String
                , serviceVersion      :: String
                }
  deriving (Show, Eq)

data ServiceId
  = ServiceId   { serviceIdVendorDomain :: String
                , serviceId             :: String
                }
  deriving (Show, Eq)
