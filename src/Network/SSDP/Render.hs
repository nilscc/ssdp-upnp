{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module Network.SSDP.Render where

import Data.List
import Network.SSDP.Types

instance Renderable ST where
  render st = case st of
    SsdpAll -> "ssdp:all"
    UpnpRootDevice -> "upnp:rootdevice"
    UuidDevice uuid -> "uuid:" ++ render uuid
    UrnDevice dom ty ver ->
      "urn:" ++ dom ++ ":device:" ++ ty ++ ":" ++ ver
    UrnService dom ty ver ->
      "urn:" ++ dom ++ ":service:" ++ ty ++ ":" ++ ver

instance Renderable UUID where
  render (UUID (a,b,c,d,e)) = intercalate "-" [a,b,c,d,e]

instance Renderable (SSDP a) where
  render ssdp = intercalate "\r\n" $
    ssdpStartingLine ssdp : map renderHeader (ssdpHeaders ssdp)
    ++ [ "\r\n" ]

renderHeader :: Header -> String
renderHeader (k :- x)       = k ++ ": " ++ x
renderHeader (k :? Just x)  = renderHeader (k :- x)
renderHeader (k :? Nothing) = k ++ ":"

instance Renderable String where
  render s = s

instance Renderable Int where
  render i = show i
