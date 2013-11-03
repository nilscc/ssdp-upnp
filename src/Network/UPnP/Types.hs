{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.UPnP.Types where

import Network.URI
import Text.XML.Light

data Device
data Service
data Actions
data Action
data Arguments

type family Upnp a
type instance Upnp Device     = UpnpXml Device
type instance Upnp Service    = UpnpXml Service
type instance Upnp Actions    = UpnpXml Actions
type instance Upnp Action     = UpnpXml Action
type instance Upnp Arguments  = UpnpXml Arguments

data UpnpXml a
  = UpnpXml { getUpnpParent     :: Maybe (UpnpParent a)
            , getUpnpURI        :: URI
            , getUpnpXmlContent :: [Element]
            }

type family UpnpParent a
type instance UpnpParent Device        = Upnp Device
type instance UpnpParent Service       = Upnp Device
type instance UpnpParent Actions       = Upnp Service
type instance UpnpParent Action        = Upnp Actions
type instance UpnpParent Arguments     = Upnp Action

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

data UpnpDataType
  = Upnp_ui1
  | Upnp_ui2
  | Upnp_ui4
  | Upnp_i1
  | Upnp_i2
  | Upnp_i4
  | Upnp_int
  | Upnp_r4
  | Upnp_r8
  | Upnp_number
  | Upnp_fixed_14_4
  | Upnp_float
  | Upnp_char
  | Upnp_string
  | Upnp_date
  | Upnp_dateTime
  | Upnp_dateTime_tz
  | Upnp_time
  | Upnp_time_tz
  | Upnp_boolean
  | Upnp_bin_base64
  | Upnp_bin_hex
  | Upnp_uri
  | Upnp_uuid
  deriving (Eq, Show)

data InOut = In | Out
  deriving (Eq, Show)

data ArgumentDesc = ArgumentDesc
  { argumentName          :: String
  , argumentDirection     :: InOut
  , argumentStateVariable :: StateVariable
  }
  deriving (Eq, Show)

data ValueRange = ValueRange
  { rangeMin  :: String
  , rangeMax  :: String
  , rangeStep :: Maybe String
  }
  deriving (Eq, Show)

data StateVariable = StateVariable
  { svarName              :: String
  , svarSendEvents        :: Bool
  , svarMulticast         :: Bool
  , svarType              :: UpnpDataType
  , svarDefault           :: Maybe String
  , svarAllowedValues     :: Maybe [String]
  , svarAllowedRange      :: Maybe ValueRange
  }
  deriving (Eq, Show)
