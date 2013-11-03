module Network.SSDP.Parser
  ( parseSsdpSearchResponse
  , parseUUID
  , parseST
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Text.ParserCombinators.Parsec hiding (many, optional, spaces)

import Network.SSDP.UUID
import Network.SSDP.Types

parseUUID :: String -> Either ParseError UUID
parseUUID = parse uuid "UUID"

parseST :: String -> Either ParseError ST
parseST = parse st "ST"

parseSsdpSearchResponse :: String -> Either ParseError (SSDP Notify)
parseSsdpSearchResponse = parse ssdpNotify "Search Response/SSDP Notify"

st :: Parser ST
st =
  choice [ SsdpAll        <$  try (string "ssdp:all")
         , UpnpRootDevice <$  try (string "upnp:rootdevice")
         , UuidDevice     <$> try (string "uuid:" *> uuid)
         , UrnDevice      <$> try (string "urn:" *> manyTill anyChar (string ":device:"))
                          <*> manyTill anyChar colon
                          <*> many anyChar
         , UrnService     <$> try (string "urn:" *> manyTill anyChar (string ":service:"))
                          <*> manyTill anyChar colon
                          <*> many anyChar
         ]

ssdpNotify :: Parser (SSDP Notify)
ssdpNotify = do
  bl   <- choice [notify, httpok]
  _    <- htmlNewLine
  hdrs <- headers
  _    <- optional htmlNewLine
  return $ SSDP bl hdrs

htmlNewLine :: Parser String
htmlNewLine = string "\r\n"

notify, httpok :: Parser String
notify  = string "NOTIFY * HTTP/1.1"
httpok  = string "HTTP/1.1 200 OK"

header :: Parser Header
header = do
  name <- manyTill anyChar colon 
  _ <- spaces
  choice [ (name :? Nothing)
            <$ lookAhead htmlNewLine
         , (name :-)
            <$> manyTill anyChar (lookAhead htmlNewLine)
         ]

headers :: Parser [Header]
headers = do
  try header `endBy` htmlNewLine

uuid :: Parser UUID
uuid = do
  a <- join <$> count 4 hexOctet
  _ <- char '-'
  b <- join <$> count 2 hexOctet
  _ <- char '-'
  c <- join <$> count 2 hexOctet
  _ <- char '-'
  d <- join <$> count 2 hexOctet
  _ <- char '-'
  e <- join <$> count 6 hexOctet
  return $ mkUUID (a,b,c,d,e)

hexOctet :: Parser [Char]
hexOctet = count 2 hexDigit <?> "octet"

colon :: Parser Char
colon = char ':' <?> ":"

spaces :: Parser ()
spaces = skipMany (satisfy (\c -> isSpace c && c /= '\n' && c /= '\r')) <?> "white space"
