module Network.UPnP.Parser where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional)

import Network.UPnP.Types

--------------------------------------------------------------------------------
-- Device

parseDeviceType :: String -> Either ParseError DeviceType
parseDeviceType = parse deviceType "DeviceType"

deviceType :: Parser DeviceType
deviceType = do
  _      <- string "urn:"

  -- either take standard constructor or get UPnP vendor for non-standard:
  constr <- choice [ StandardDevice
                      <$ try (string "schemas-upnp-org:device:")
                   , NonStandardDevice
                      <$> manyTill anyChar (string ":device:")
                   ]

  -- apply type + version to constructor
  constr <$> manyTill anyChar colon
         <*> many anyChar

--------------------------------------------------------------------------------
-- Service

parseServiceType :: String -> Either ParseError ServiceType
parseServiceType = parse serviceType "ServiceType"

serviceType :: Parser ServiceType
serviceType = do
  _ <- string "urn:"

  -- pick constructor (see `deviceType`)
  constr <- choice [ StandardService
                      <$ try (string "schemas-upnp-org:service:")
                   , NonStandardService
                      <$> manyTill anyChar (string ":service:")
                   ]
  constr <$> manyTill anyChar colon
         <*> many anyChar

parseServiceId :: String -> Either ParseError ServiceId
parseServiceId = parse serviceId "ServiceId"

serviceId :: Parser ServiceId
serviceId = do
  _ <- string "urn:"
  -- pick constructor (see `deviceType`)
  constr <- choice [ StandardServiceId
                      <$ try (string "upnp-org:serviceId:")
                   , NonStandardServiceId
                      <$> manyTill anyChar (string ":serviceId:")
                   ]
  constr <$> many anyChar

--------------------------------------------------------------------------------
-- Helper

colon :: Parser Char
colon = char ':'
