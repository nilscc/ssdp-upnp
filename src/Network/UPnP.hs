{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}

module Network.UPnP
  ( -- * Device description
    requestDeviceDescription
  , Upnp, getUpnpParent, getUpnpURI
    -- ** Devices
  , Device
  , getDeviceType, DeviceType (..)
  , getFriendlyName, getManufacturer, getModelName, getUDN
  , getDeviceList
    -- ** Services
  , Service
  , getServiceList
  , getServiceType, ServiceType (..), standardService
  , getServiceId, ServiceId (..)
  , getSCPDURL, getControlURL, getEventSubURL
  , findService
    -- ** Actions
  , Actions
  , requestActions
  , getActionNames
  , getActionByName
  , getArguments
  , ArgumentDesc (..), StateVariable (..), InOut(..), ValueRange (..)
    -- ** Other
  , getStringValue, getRequiredStringValue
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Error
import Data.Maybe
import Data.Monoid
import Text.XML.Light
import Network.URI
import Network.HTTP

import Network.SSDP
import Network.UPnP.Types
import Network.UPnP.Parser

require :: Monad m => Maybe a -> ErrorT String m a
require = maybe (fail "Unexpected Nothing") return 

requireRight :: Monad m => Either err a -> ErrorT String m a
requireRight = either (\_ -> fail "Unexpected Left") return

--------------------------------------------------------------------------------
-- Device description

requestDeviceDescription :: SSDP Notify -> IO (Either String (Upnp Device))
requestDeviceDescription ssdp = runErrorT $ do
  loc <- require $ getHeaderValue "LOCATION" ssdp
  uri <- case parseURI loc of
           Just uri -> return uri { uriPath = "/" }
           Nothing  -> fail "Invalid location."
  res <- liftIO $ simpleHTTP (getRequest loc)
  bdy <- rspBody <$> requireRight res
  let xml  = parseXML bdy
      els  = onlyElems xml
      devs = pickChildren (hasElName "device") $ els
  case devs of
    [dev] -> return $ UpnpXml Nothing uri [dev]
    _     -> fail "Unexpected number of <device> tags."
 where

--------------------------------------------------------------------------------
-- Helper

hasElName :: String -> Element -> Bool
hasElName n (elName -> (qName -> n')) = n == n'

hasValue :: String -> Element -> Bool
hasValue v (elContent -> [Text (cdData -> v')]) = v == v'
hasValue _ _ = False

infixr 9 ~>
(~>) :: (a -> b) -> (b -> c) -> a -> c
(~>) = flip (.)

pickChildren :: (Element -> Bool) -> [Element] -> [Element]
pickChildren test = concatMap (filterChildren test)

pickTag :: String -> [Element] -> [Element]
pickTag = pickChildren . hasElName

pickStringValues :: [Element] -> [String]
pickStringValues els =
  [ cdData d | el <- els
             , Text d <- elContent el
             ]

-- | Find a optional string valued device property
getStringValue :: String -> UpnpXml a -> Maybe String
getStringValue s (UpnpXml _ _ dev) =
  case pickChildren (hasElName s) dev of
    [elContent -> [Text (cdData -> str)]] -> Just str
    _ -> Nothing

-- | Find a required string valued device property. Uses `fail` on error.
getRequiredStringValue :: String -> UpnpXml a -> String
getRequiredStringValue s =
    maybe (fail $ "Tag <" ++ s ++ "> or its value not found.") id
  . getStringValue s

-- | Filter elements which contain a certain @<name>@ tag value
filterByNameValue :: String -> [Element] -> [Element]
filterByNameValue val = filter $
  or . map (\el -> hasElName "name" el && hasValue val el)
     . onlyElems
     . elContent

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
getDeviceList upnp@(UpnpXml _ uri dev) =
  map (UpnpXml (Just upnp) uri . return) $
     pickTag "deviceList"
  ~> pickTag "device"
   $ dev

--------------------------------------------------------------------------------
-- Services

-- | Standard device type with \"schemas-upnp-org\" as vendor
standardService :: String -> String -> ServiceType
standardService = ServiceType "schemas-upnp-org"

getServiceList :: Upnp Device -> [Upnp Service]
getServiceList upnp@(UpnpXml _ uri dev) = map (UpnpXml (Just upnp) uri . return) $
     pickTag "serviceList"
  ~> pickTag "service"
   $ dev

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
  :: Upnp Service -> URI
getSCPDURL     = toUriUnsafe . getRequiredStringValue "SCPDURL"
getControlURL  = toUriUnsafe . getRequiredStringValue "controlURL"
getEventSubURL = toUriUnsafe . getRequiredStringValue "eventSubURL"

toUriUnsafe :: String -> URI
toUriUnsafe = fromJust . parseURIReference

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

--------------------------------------------------------------------------------
-- Actions

requestActions
  :: Upnp Service -> IO (Either String (Upnp Actions))
requestActions service@(UpnpXml _ uri _) = runErrorT $ do
  let scpduri = getSCPDURL service `relativeTo` uri
  res <- liftIO $ simpleHTTP $ getRequest $ show scpduri
  bdy <- rspBody <$> requireRight res
  let xml     = parseXML bdy
      els     = onlyElems xml
  return $ UpnpXml (Just service) scpduri els

-- helper:
pickActions :: [Element] -> [Element]
pickActions =
     pickTag "actionList"
  ~> pickTag "action"

-- | Get all names of the supported actions of a service
getActionNames :: Upnp Actions -> [String]
getActionNames (UpnpXml _ _ xml) =
     pickActions
  ~> pickTag "name"
  ~> pickStringValues
   $ xml

-- helper:
pickActionByName :: String -> [Element] -> [Element]
pickActionByName name =
     pickActions
  ~> filter (or . map (\el -> hasElName "name" el && hasValue name el)
                . onlyElems . elContent)

getActionByName :: String -> Upnp Actions -> Maybe (Upnp Action)
getActionByName name actions@(UpnpXml _ uri xml) =
  case pickActionByName name xml of
    action@[_] -> Just $ UpnpXml (Just actions) uri action
    _          -> Nothing

-- helper:
pickArguments :: [Element] -> [Element]
pickArguments =
     pickTag "argumentList"
  ~> pickTag "argument"

-- helper:
pickStateVariables :: [Element] -> [Element]
pickStateVariables =
     pickTag "serviceStateTable"
  ~> pickTag "stateVariable"

getStateVariableByName :: String -> Upnp Actions -> Maybe StateVariable
getStateVariableByName name (UpnpXml _ _ xml) =
     pickStateVariables
  ~> filterByNameValue name
  ~> map toStateVar
  ~> listToMaybe
   $ xml
 where
  toStateVar svar =
    let -- name value
        nval = pickTag "name" ~> pickStringValues ~> concat $ [svar]
        -- data type
        daty = case pickTag "dataType" ~> pickStringValues ~> concat $ [svar] of
                 "ui1"        -> Upnp_ui1
                 "ui2"        -> Upnp_ui2
                 "ui4"        -> Upnp_ui4
                 "i1"         -> Upnp_i1
                 "i2"         -> Upnp_i2
                 "i4"         -> Upnp_i4
                 "int"        -> Upnp_int
                 "r4"         -> Upnp_r4
                 "r8"         -> Upnp_r8
                 "number"     -> Upnp_number
                 "fixed_14_4" -> Upnp_fixed_14_4
                 "float"      -> Upnp_float
                 "char"       -> Upnp_char
                 "string"     -> Upnp_string
                 "date"       -> Upnp_date
                 "dateTime"   -> Upnp_dateTime
                 "dateTime_tz"-> Upnp_dateTime_tz
                 "time"       -> Upnp_time
                 "time_tz"    -> Upnp_time_tz
                 "boolean"    -> Upnp_boolean
                 "bin_base64" -> Upnp_bin_base64
                 "bin_hex"    -> Upnp_bin_hex
                 "uri"        -> Upnp_uri
                 "uuid"       -> Upnp_uuid
                 _            -> Upnp_string

        -- "sendEvents" (default "yes") attribute value:
        sendEvents = not $ or [ qName k == "sendEvents" && v == "no"
                                | Attr k v <- elAttribs svar ]

        -- "multicast" (default "no") attribute value:
        multicast  =       or [ qName k == "multicast"  && v == "yes"
                                | Attr k v <- elAttribs svar ]
                        
     in StateVariable
          { svarName          = nval
          , svarSendEvents    = sendEvents
          , svarMulticast     = multicast
          , svarType          = daty
          , svarDefault       = Nothing -- TODO
          , svarAllowedValues = Nothing -- TODO
          , svarAllowedRange  = Nothing -- TODO
          }

getArguments :: Upnp Action -> [ArgumentDesc]
getArguments (UpnpXml (Just actions) _ (pickArguments -> xml)) =

  let names      = pickTag "name"      ~> pickStringValues
                 $ xml
      directions = pickTag "direction" ~> pickStringValues ~> map inOrOut
                 $ xml
      relsvars   = pickTag "relatedStateVariable" ~> pickStringValues
                 $ xml

   in [ ArgumentDesc name dir sv
        | name <- names
        | dir <- directions
        | relsv <- relsvars
        , let Just sv = getStateVariableByName relsv actions
        ]

 where
  inOrOut "in"  = In
  inOrOut "out" = Out
  inOrOut _     = In -- should hopefully never happen -> FIXME?

getArguments _ = []
