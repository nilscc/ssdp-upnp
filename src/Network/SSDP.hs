{-# LANGUAGE OverloadedStrings #-}

module Network.SSDP
  ( SSDP
  , ST (..)
  , Header (..)
  , UUID, generateUUID, mkUUID
  , renderSSDP
  , ssdpDiscover
  ) where

import Data.List
import Data.Maybe

import Network.SSDP.UUID

data SSDP = SSDP
  { ssdpRequestLine :: String
  , ssdpHeaders :: [Header]
  }
  deriving Show

data Header
  = String :- String
  | String :? Maybe String
  deriving Show

type UserAgent = String
type MX = Int

--------------------------------------------------------------------------------
-- SSDP Messages

data ST
  = SsdpAll
  | UpnpRootDevice
  | UuidDevice UUID
  | UrnDevice  { deviceDomain  :: String
               , deviceType    :: String
               , deviceVersion :: String }
  | UrnService { serviceDomain  :: String
               , serviceType    :: String
               , serviceVersion :: String }

ssdpDiscover
  :: ST
  -> Maybe MX
  -> Maybe UserAgent
  -> SSDP
ssdpDiscover st mx mua = SSDP
  "M-SEARCH * HTTP/1.1"
  [ "Host"      :- "239.255.255.250:1900"
  , "Man"       :- "\"ssdp:discover\""
  , "ST"        :- renderST st
  , "MX"        :- maybe "3" show mx
  , "UserAgent" :? mua
  ]

--------------------------------------------------------------------------------
-- Rendering

renderSSDP :: SSDP -> String
renderSSDP ssdp = intercalate "\r\n" $
  [ ssdpRequestLine ssdp ]
  ++ mapMaybe renderHeader (ssdpHeaders ssdp)
  ++ [ "\r\n" ]

renderHeader :: Header -> Maybe String
renderHeader (k :- x)       = Just $ k ++ ": " ++ x
renderHeader (k :? Just x)  = renderHeader (k :- x)
renderHeader (_ :? Nothing) = Nothing

renderST :: ST -> String
renderST st = case st of
  SsdpAll -> "ssdp:all"
  UpnpRootDevice -> "upnp:rootdevice"
  UuidDevice uuid -> "uuid:" ++ renderUuid uuid
  UrnDevice dom ty ver ->
    "urn:" ++ dom ++ ":device:" ++ ty ++ ":" ++ ver
  UrnService dom ty ver ->
    "urn:" ++ dom ++ ":service:" ++ ty ++ ":" ++ ver
