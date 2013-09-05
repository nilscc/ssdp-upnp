module Network.SSDP.Parser
  ( parseSsdpSearchResponse
  , parseUUID
  ) where

import Control.Applicative
import Control.Monad
import Text.ParserCombinators.Parsec hiding (many)

import Network.SSDP.UUID
import Network.SSDP.Types

parseUUID :: String -> Either ParseError UUID
parseUUID = parse uuid "UUID"

parseSsdpSearchResponse :: String -> Either ParseError (SSDP Notify)
parseSsdpSearchResponse = parse ssdpNotify "Search Response/SSDP Notify"

ssdpNotify :: Parser (SSDP Notify)
ssdpNotify = do
  bl   <- choice [notify, httpok]
  _    <- htmlNewLine
  hdrs <- headers
  _    <- htmlNewLine
  return $ SSDP bl hdrs

htmlNewLine :: Parser String
htmlNewLine = string "\r\n"

notify, httpok :: Parser String
notify  = string "NOTIFY * HTTP/1.1"
--msearch = string "M-SEARCH * HTTP/1.1"
httpok  = string "HTTP/1.1 200 OK"

--startLine :: Parser String
--startLine = choice [ notify, msearch, httpok ]

header :: Parser Header
header = do
  name <- manyTill anyChar colon 
  _ <- spaces
  val <- manyTill anyChar $ lookAhead htmlNewLine
  return $ name :- val

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
hexOctet = count 2 hexDigit

colon :: Parser Char
colon = char ':'
