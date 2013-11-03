{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

module Network.SSDP
  ( -- * SSDP related types
    SSDP, Search, Notify
  , ST (..)
  , Header (..), HasHeader (..)
  , UserAgent, MX, MaxAge, Location, Server, BootId, ConfigId, Searchport
  , UUID, generateUUID, mkUUID
    -- * SSDP messages
  , ssdpSearch, ssdpSearchResponse
    -- ** SSDP headers
  , getHeaderValue, hasHeader
    -- * Sending & receiving SSDP messages
  , sendNotify, sendSearch
  , Renderable (..)
  , ParseError
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception    as E
import           Control.Monad
import           Control.Monad.Trans

import           Data.Char
import           Data.Maybe
import           Data.List
import           Text.ParserCombinators.Parsec (ParseError)

import           Network
import qualified Network.Socket       as S
import           Network.Multicast
import           Network.SSDP.Parser
import           Network.SSDP.Headers ()
import           Network.SSDP.Render ()
import           Network.SSDP.Types
import           Network.SSDP.UUID

--------------------------------------------------------------------------------
-- Sending & receiving SSDP messages

ssdpPort :: S.PortNumber
ssdpPort = 1900

ssdpAddr :: HostName
ssdpAddr = "239.255.255.250"

sendNotify :: MonadIO m => SSDP Notify -> m ()
sendNotify ssdp = liftIO $ do

  -- send ssdp:discover
  (sock, sockaddr) <- multicastSender ssdpAddr ssdpPort
  _ <- S.sendTo sock (render ssdp) sockaddr
  S.sClose sock

sendSearch
  :: MonadIO m
  => SSDP Search
  -> m ( [(S.SockAddr, SSDP Notify)]
       , [(S.SockAddr, ParseError)]
       )
sendSearch ssdp = liftIO $ do

  results <- newTChanIO
  errors  <- newTChanIO

  (sock, sockaddr) <- multicastSender ssdpAddr ssdpPort
  _ <- S.sendTo sock (render ssdp) sockaddr

  let mx = get MXH ssdp

#ifdef mingw32_HOST_OS
  S.setSocketOption sock S.RecvTimeOut (mx * 1000)
#endif

  let loop = forever $ do

        -- receive & parse (TODO) incoming NOTIFY message
        (msg, _, from) <- S.recvFrom sock 4096
        putStrLn $ "Message from " ++ show from

        atomically $
          case parseSsdpSearchResponse msg of
            -- store result
            Right notify -> writeTChan results (from, notify)
            Left  err    -> writeTChan errors  (from, err)

  -- set timeout & start looping
  killAfter (mx * 1000 * 1000) loop `E.finally` S.sClose sock

  -- wait for all threads to finish before returning all results
  atomically $ (,) <$> chanToList results
                   <*> chanToList errors
 where
  killAfter n io = do
    tid <- forkIO $ io
    threadDelay n
    killThread tid

  chanToList chan = do
    isempty <- isEmptyTChan chan
    if isempty
       then return []
       else (:) <$> readTChan chan
                <*> chanToList chan


--------------------------------------------------------------------------------
-- SSDP headers

getHeaderValue :: String -> SSDP a -> Maybe String
getHeaderValue (map toUpper -> hdr) (SSDP _ hdrs) =
  join $ hdrVal <$> find ((hdr ==) . hdrName) hdrs
 where

hdrName :: Header -> String
hdrName (h :- _) = map toUpper h
hdrName (h :? _) = map toUpper h

hdrVal :: Header -> Maybe String
hdrVal (_ :- v) = Just v
hdrVal (_ :? v) = v

hasHeader :: String -> SSDP a -> Bool
hasHeader (map toUpper -> hdr) (SSDP _ hdrs) = go hdrs
 where
  go [] = False
  go (x:xs) = if hdr == hdrName x then True else go xs

--------------------------------------------------------------------------------
-- SSDP Messages

ssdpSearch
  :: ST
  -> Maybe MX
  -> Maybe UserAgent
  -> SSDP Search
ssdpSearch st mmx mua = SSDP
  "M-SEARCH * HTTP/1.1"
  [ "HOST"      :- "239.255.255.250:1900"
  , "MAN"       :- "\"ssdp:discover\""
  , "ST"        :- render st
  , "MX"        :- render mx
  , "USERAGENT" :? fmap render mua
  ]
 where
  mx = fromMaybe 3 mmx

ssdpSearchResponse
  :: Location
  -> Server
  -> MaxAge
  -> ST
  -> UUID
  -> Maybe BootId
  -> Maybe ConfigId
  -> Maybe Searchport
  -> SSDP Response
ssdpSearchResponse loc srv maxAge st uuid mbid mcid msp = SSDP
  "HTTP/1.1 200 OK"
  [ "LOCATION"              :- render loc
  , "SERVER"                :- render srv
  , "CACHE-CONTROL"         :- "max-age=" ++ render maxAge
  , "EXT"                   :- ""
  , "ST"                    :- render st
  , "USN"                   :- "uuid:" ++ render uuid ++ "::" ++ render st
  , "BOOTID.UPNP.ORG"       :? fmap render mbid
  , "CONFIGID.UPNP.ORG"     :? fmap render mcid
  , "SEARCHPORT.UPNP.ORG"   :? fmap render msp
  ]
