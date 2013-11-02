{-# LANGUAGE EmptyDataDecls #-}

module Network.UPnP.Types where

import Text.XML.Light

newtype Upnp a = UpnpXml { getUpnpXmlContent :: Element }
  deriving (Show)

data Device
data Service

data DeviceType
  = StandardDevice           String String
  | NonStandardDevice String String String
  deriving (Show, Eq)

data ServiceType
  = StandardService           String String
  | NonStandardService String String String
  deriving (Show, Eq)

data ServiceId
  = StandardServiceId           String
  | NonStandardServiceId String String
  deriving (Show, Eq)
