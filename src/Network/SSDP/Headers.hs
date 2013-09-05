{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}

module Network.SSDP.Headers where

import Control.Monad
import Data.Char
import Data.List
import Network.SSDP.Types
import Network.SSDP.Parser

findHeaderByName :: String -> [Header] -> Maybe Header
findHeaderByName hdr hdrs = find f hdrs
 where
  f (k :- _) = map toLower k == map toLower hdr
  f (k :? _) = map toLower k == map toLower hdr

getHeaderValue :: Header -> Maybe String
getHeaderValue (_ :- v) = Just v
getHeaderValue (_ :? v) = v

findHeaderValueByName :: String -> [Header] -> Maybe String
findHeaderValueByName name =
  join . fmap getHeaderValue . findHeaderByName name


--------------------------------------------------------------------------------
-- Specific SSDP headers

instance HasHeader (SSDP Search) HostH String where
  get _ (SSDP _ hdrs) = 
    let Just hst = findHeaderValueByName "HOST" hdrs
     in hst

instance HasHeader (SSDP Search) ManH String where
  get _ (SSDP _ hdrs) = 
    let Just mn = findHeaderValueByName "MAN" hdrs
     in mn

instance HasHeader (SSDP Search) MXH Int where
  get _ (SSDP _ hdrs) = 
    let Just mx = findHeaderValueByName "MX" hdrs
     in read mx

instance HasHeader (SSDP Search) STH (Maybe ST) where
  get _ (SSDP _ hdrs) =
    let Just s   = findHeaderValueByName "ST" hdrs
     in case parseST s of
             Right st -> Just st
             _        -> Nothing

instance HasHeader (SSDP Search) UserAgentH (Maybe String) where
  get _ (SSDP _ hdrs) = 
    let ua = findHeaderValueByName "USERAGENT" hdrs
     in ua


