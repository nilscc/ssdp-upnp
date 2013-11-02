module Network.UPnP.Types where

import Text.XML.Light

newtype UpnpXml = UpnpXml { getUpnpXmlContent :: [Content] }
  deriving (Show)

type DeviceDescription = UpnpXml
