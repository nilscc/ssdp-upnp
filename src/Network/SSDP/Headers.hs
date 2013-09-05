module Network.SSDP.Headers where

import Control.Monad
import Data.Char
import Data.List
import Network.SSDP.Types

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
