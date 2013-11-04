{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

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
    -- * Control
  , prepareStatement
    -- ** Other
  , getStringValue, getRequiredStringValue
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Error
import Data.Maybe
import Data.Monoid
import Data.List
import Text.XML.Light
import Network.URI
import Network.HTTP

import Network.SSDP
import Network.UPnP.Types
import Network.UPnP.Parser
import Network.UPnP.Render

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
    [dev] -> return $ UpnpXml Nothing uri Nothing Nothing [dev]
    _     -> fail "Unexpected number of <device> tags."
 where

--------------------------------------------------------------------------------
-- Helper

hasElName :: String -> Element -> Bool
hasElName n (elName -> (qName -> n')) = n == n'

hasValue :: String -> Element -> Bool
hasValue v (elContent -> [Text (cdData -> v')]) = v == v'
hasValue _ _ = False

hasEl :: (Element -> Bool) -> Element -> Bool
hasEl test el = or . map test $ elChildren el

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
getStringValue s (UpnpXml _ _ _ _ dev) =
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
getDeviceList upnp@(UpnpXml _ uri _ _ dev) =
  map (UpnpXml (Just upnp) uri Nothing Nothing . return) $
     pickTag "deviceList"
  ~> pickTag "device"
   $ dev

--------------------------------------------------------------------------------
-- Services

-- | Standard device type with \"schemas-upnp-org\" as vendor
standardService :: String -> String -> ServiceType
standardService = ServiceType "schemas-upnp-org"

getServiceList :: Upnp Device -> [Upnp Service]
getServiceList upnp@(UpnpXml _ uri _ _ dev) =
  map (UpnpXml (Just upnp) uri Nothing Nothing . return) $
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
requestActions service@(UpnpXml _ uri _ _ _) = runErrorT $ do
  let scpduri = getSCPDURL service `relativeTo` uri
  res <- liftIO $ simpleHTTP $ getRequest $ show scpduri
  bdy <- rspBody <$> requireRight res
  let xml     = parseXML bdy
      els     = onlyElems xml
  return $ UpnpXml (Just service) scpduri Nothing Nothing els

-- helper:
pickActions :: [Element] -> [Element]
pickActions =
     pickTag "actionList"
  ~> pickTag "action"

-- | Get all names of the supported actions of a service
getActionNames :: Upnp Actions -> [String]
getActionNames (UpnpXml _ _ _ _ xml) =
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
getActionByName name actions@(UpnpXml _ uri _ _ xml) =
  case pickActionByName name xml of
    action@[_] -> Just $ UpnpXml (Just actions) uri Nothing (Just name) action
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
getStateVariableByName name (UpnpXml _ _ _ _ xml) =
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

        -- default value
        defval
          | hasEl (hasElName "defaultValue") svar = Just $
            pickTag "defaultValue" ~> pickStringValues ~> concat $ [svar]
          | otherwise = Nothing

        -- allowed values
        allowed
          | hasEl (hasElName "allowedValueList") svar = Just $
               pickTag "allowedValueList"
            ~> pickTag "allowedValue"
            ~> pickStringValues
             $ [svar]
          | otherwise = Nothing

        -- range values
        range
          | hasEl (hasElName "allowedValueRange") svar = Just $
               pickTag "allowedValueRange"
            ~> buildRange
             $ [svar]
          | otherwise = Nothing
         where
          buildRange els = ValueRange 
                             { rangeMin  = pickTag "minimum"
                                        ~> pickStringValues ~> concat
                                         $ els
                             , rangeMax  = pickTag "maximum"
                                        ~> pickStringValues ~> concat
                                         $ els
                             , rangeStep = if or [ hasEl (hasElName "step") el | el <- els ]
                                             then Just $ pickTag "step"
                                                      ~> pickStringValues ~> concat
                                                       $ els
                                             else Nothing
                             }

     in StateVariable
          { svarName          = nval
          , svarSendEvents    = sendEvents
          , svarMulticast     = multicast
          , svarType          = daty
          , svarDefault       = defval
          , svarAllowedValues = allowed
          , svarAllowedRange  = range
          }

getArguments :: Upnp Action -> [ArgumentDesc]
getArguments (UpnpXml (Just actions) _ _ _ (pickArguments -> xml)) =

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
          -- use do notation to avoid irrefutable pattern errors.
          -- invalid arguments (with missing state variables) will get ignored
          -- this way
        , Just sv <- return $ getStateVariableByName relsv actions
        ]

 where
  inOrOut "in"  = In
  inOrOut "out" = Out
  inOrOut _     = In -- should hopefully never happen -> FIXME?

getArguments _ = []


--------------------------------------------------------------------------------
-- Control

-- | Build a statement without argument verification
buildStatement :: Upnp Action -> [Argument] -> Maybe (Upnp Statement)
buildStatement action@(UpnpXml par uri (Just sty) (Just aname) _) args
  | Just (service :: Upnp Service) <- par >>= getUpnpParent
  = let
        control :: URI
        control = getControlURL service

        -- action invocation!
        invoc :: [Element]
        invoc = renderArguments sty aname args

     in Just $ UpnpXml (Just action) (control `relativeTo` uri) Nothing Nothing invoc

buildStatement _ _ = Nothing

-- | Prepare a statement with argument verification
prepareStatement :: Upnp Action -> [Argument] -> Maybe (Upnp Statement)
prepareStatement action args

  | and [ validArg (getArguments action) arg | arg <- args ] =
    buildStatement action args

  | otherwise =
    Nothing

 where

  -- Check if an argument matches the given description
  validArg :: [ArgumentDesc] -> Argument -> Bool
  validArg descs (Argument name val)

      -- first, find the description with the right name
    | Just desc <- find (\d -> argumentDescName d == name) descs
    , let svar = argumentDescStateVariable desc
      -- only accept "in" arguments
    , argumentDescDirection desc == In
      -- make sure the types match
    , typeMatches (svarType svar) val
      -- make sure only allowed values (if any) are used
    , valueAllowed (svarAllowedValues svar) val
      -- TODO: check value range
    = True

    | otherwise
    = False

  typeMatches ty val = case (ty, val) of
      (Upnp_ui1       , UpnpVal_ui1         _) -> True
      (Upnp_ui2       , UpnpVal_ui2         _) -> True
      (Upnp_ui4       , UpnpVal_ui4         _) -> True
      (Upnp_i1        , UpnpVal_i1          _) -> True
      (Upnp_i2        , UpnpVal_i2          _) -> True
      (Upnp_i4        , UpnpVal_i4          _) -> True
      (Upnp_int       , UpnpVal_int         _) -> True
      (Upnp_r4        , UpnpVal_r4          _) -> True
      (Upnp_r8        , UpnpVal_r8          _) -> True
      (Upnp_number    , UpnpVal_number      _) -> True
      (Upnp_fixed_14_4, UpnpVal_fixed_14_4  _) -> True
      (Upnp_float     , UpnpVal_float       _) -> True
      (Upnp_char      , UpnpVal_char        _) -> True
      (Upnp_string    , UpnpVal_string      _) -> True
      (Upnp_boolean   , UpnpVal_boolean     _) -> True
      (Upnp_bin_base64, UpnpVal_bin_base64  _) -> True
      (Upnp_bin_hex   , UpnpVal_bin_hex     _) -> True
      (Upnp_uri       , UpnpVal_uri         _) -> True
      (Upnp_uuid      , UpnpVal_uuid        _) -> True
      _ -> False

  valueAllowed (Just vals) val = render val `elem` vals
  valueAllowed Nothing     _   = True
