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

instance HasHeader (SSDP Search) Host where
  get (SSDP _ hdrs) = 
    let Just hst = findHeaderValueByName "HOST" hdrs
     in Host hst

instance HasHeader (SSDP Search) Man where
  get (SSDP _ hdrs) = 
    let Just mn = findHeaderValueByName "MAN" hdrs
     in Man mn

instance HasHeader (SSDP Search) MX where
  get (SSDP _ hdrs) = 
    let Just mx = findHeaderValueByName "MX" hdrs
     in MX $ read mx

instance HasHeader (SSDP Search) (Maybe ST) where
  get (SSDP _ hdrs) =
    let Just s   = findHeaderValueByName "ST" hdrs
     in case parseST s of
             Right st -> Just st
             _        -> Nothing

instance HasHeader (SSDP Search) (Maybe UserAgent) where
  get (SSDP _ hdrs) = 
    let ua = findHeaderValueByName "USERAGENT" hdrs
     in fmap UserAgent ua


