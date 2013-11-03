module Network.UPnP.Parser where

import Control.Applicative
import Control.Monad
import Text.ParserCombinators.Parsec hiding (many, optional)

import Network.UPnP.Types

--------------------------------------------------------------------------------
-- Device

parseDeviceType :: String -> Either ParseError DeviceType
parseDeviceType = parse pDeviceType "DeviceType"

pDeviceType :: Parser DeviceType
pDeviceType = do
  void $ string "urn:"
  -- either take standard constructor or get UPnP vendor for non-standard:
  DeviceType <$> manyTill anyChar (string ":device:") -- vendor
             <*> manyTill anyChar colon               -- dev ty
             <*> many anyChar                         -- version

--------------------------------------------------------------------------------
-- Service

parseServiceType :: String -> Either ParseError ServiceType
parseServiceType = parse pServiceType "ServiceType"

pServiceType :: Parser ServiceType
pServiceType = do
  void $ string "urn:"
  -- pick constructor (see `deviceType`)
  ServiceType <$> manyTill anyChar (string ":service:")
              <*> manyTill anyChar colon
              <*> many anyChar

parseServiceId :: String -> Either ParseError ServiceId
parseServiceId = parse pServiceId "ServiceId"

pServiceId :: Parser ServiceId
pServiceId = do
  void $ string "urn:"
  -- pick constructor (see `deviceType`)
  ServiceId <$> manyTill anyChar (string ":serviceId:")
            <*> many anyChar

--------------------------------------------------------------------------------
-- Helper

colon :: Parser Char
colon = char ':'
