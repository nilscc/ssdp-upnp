{-# LANGUAGE OverloadedStrings #-}

module Network.SSDP
  ( -- * SSDP related types
    SSDP
  , ST (..)
  , Header (..)
  , UserAgent, MX, MaxAge, Location, Server, BootId, ConfigId, Searchport
  , UUID, generateUUID, mkUUID
    -- * SSDP messages
  , ssdpSearch, ssdpSearchResponse
    -- * Rendering
  , renderSSDP
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

infixr 0 :-
infixr 0 :?

type UserAgent = String
type MX = Int

type MaxAge = Int
type Location = String
type Server = String

type BootId = Int
type ConfigId = Int
type Searchport = Int

--------------------------------------------------------------------------------
-- SSDP Messages

-- | SSDP Search targets
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

ssdpSearch
  :: ST
  -> Maybe MX
  -> Maybe UserAgent
  -> SSDP
ssdpSearch st mx mua = SSDP
  "M-SEARCH * HTTP/1.1"
  [ "Host"      :- "239.255.255.250:1900"
  , "Man"       :- "\"ssdp:discover\""
  , "ST"        :- renderST st
  , "MX"        :- maybe "3" show mx
  , "UserAgent" :? mua
  ]

ssdpSearchResponse
  :: Location
  -> Server
  -> MaxAge
  -> ST
  -> UUID
  -> Maybe BootId
  -> Maybe ConfigId
  -> Maybe Searchport
  -> SSDP
ssdpSearchResponse loc srv maxAge st uuid mbid mcid msp = SSDP
  "HTTP/1.1 200 OK"
  [ "LOCATION"              :- loc
  , "SERVER"                :- srv
  , "CACHE-CONTROL"         :- "max-age=" ++ show maxAge
  , "EXT"                   :- ""
  , "ST"                    :- renderST st
  , "USN"                   :- "uuid:" ++ renderUuid uuid ++ "::" ++ renderST st
  , "BOOTID.UPNP.ORG"       :? fmap show mbid
  , "CONFIGID.UPNP.ORG"     :? fmap show mcid
  , "SEARCHPORT.UPNP.ORG"   :? fmap show msp
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
