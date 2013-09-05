{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.SSDP
  ( -- * SSDP related types
    SSDP, Search, Notify
  , ST (..)
  , Header (..)
  , UserAgent, MX, MaxAge, Location, Server, BootId, ConfigId, Searchport
  , UUID, generateUUID, mkUUID
    -- * SSDP messages
  , ssdpSearch, ssdpSearchResponse
  , getMX
    -- * Sending & receiving SSDP messages
  , sendNotify, sendSearch
    -- * Rendering
  , renderSSDP
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception    as E
import           Control.Monad
import           Control.Monad.Trans

import           Data.List
import           Data.Maybe

import           Network
import qualified Network.Socket       as S
import           Network.Multicast
import           Network.SSDP.Parser
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
  _ <- S.sendTo sock (renderSSDP ssdp) sockaddr
  S.sClose sock

sendSearch
  :: MonadIO m
  => SSDP Search
  -> (S.SockAddr -> SSDP Notify -> IO a) -- ^ callback for replies
  -> m [a]
sendSearch ssdp callback = liftIO $ do

  (sock, sockaddr) <- multicastSender ssdpAddr ssdpPort
  _ <- S.sendTo sock (renderSSDP ssdp) sockaddr

  results <- newTChanIO
  count   <- newTVarIO (0 :: Int)

  let mx = getMX ssdp

  S.setSocketOption sock S.RecvTimeOut (mx * 1000)

  let

      runCallback from notify = do

        -- "remember" thread
        atomically $ modifyTVar count (+1)

        -- run callback & store result
        result <- callback from notify
        atomically $ do
          writeTChan results result
          -- remove current thread
          modifyTVar count (`subtract` 1)
  
      loop = do

        -- receive & parse (TODO) incoming NOTIFY message
        (msg, _, from) <- S.recvFrom sock 4096

        case parseSsdpSearchResponse msg of

             Right notify -> void $ forkIO $
               runCallback from notify

             Left err     -> print err -- FIXME

        loop

  -- set timeout & start looping
  killAfter (mx * 1000 * 1000) loop `E.finally` S.sClose sock

  -- wait for all threads to finish before returning all results
  atomically $ do
    c <- readTVar count
    when (c > 0) retry
    chanToList results

 where
  killAfter n io = do
    tid <- forkIO $ io
    threadDelay n
    killThread tid



--------------------------------------------------------------------------------
-- SSDP Messages

getMX :: SSDP Search -> MX
getMX (SSDPSearch mx _ _) = mx

getStartLine :: SSDP a -> String
getStartLine (SSDPSearch _ rl _) = rl
getStartLine (SSDPNotify   rl _) = rl
getStartLine (SSDPResponse rl _) = rl

getHeaders :: SSDP a -> [Header]
getHeaders (SSDPSearch _ _ h) = h
getHeaders (SSDPNotify   _ h) = h
getHeaders (SSDPResponse _ h) = h

ssdpSearch
  :: ST
  -> Maybe MX
  -> Maybe UserAgent
  -> SSDP Search
ssdpSearch st mmx mua = SSDPSearch mx
  "M-SEARCH * HTTP/1.1"
  [ "HOST"      :- "239.255.255.250:1900"
  , "MAN"       :- "\"ssdp:discover\""
  , "ST"        :- renderST st
  , "MX"        :- show mx
  , "USERAGENU" :? mua
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
ssdpSearchResponse loc srv maxAge st uuid mbid mcid msp = SSDPResponse
  "HTTP/1.1 200 OK"
  [ "LOCATION"              :- loc
  , "SERVER"                :- srv
  , "CACHE-CONTROL"         :- "max-age=" ++ show maxAge
  , "EXT"                   :- ""
  , "ST"                    :- renderST st
  , "USN"                   :- "uuid:" ++ renderUUID uuid ++ "::" ++ renderST st
  , "BOOTID.UPNP.ORG"       :? fmap show mbid
  , "CONFIGID.UPNP.ORG"     :? fmap show mcid
  , "SEARCHPORT.UPNP.ORG"   :? fmap show msp
  ]

--------------------------------------------------------------------------------
-- Rendering

renderSSDP :: SSDP a -> String
renderSSDP ssdp = intercalate "\r\n" $
  [ getStartLine ssdp ]
  ++ mapMaybe renderHeader (getHeaders ssdp)
  ++ [ "\r\n" ]

renderHeader :: Header -> Maybe String
renderHeader (k :- x)       = Just $ k ++ ": " ++ x
renderHeader (k :? Just x)  = renderHeader (k :- x)
renderHeader (_ :? Nothing) = Nothing

renderST :: ST -> String
renderST st = case st of
  SsdpAll -> "ssdp:all"
  UpnpRootDevice -> "upnp:rootdevice"
  UuidDevice uuid -> "uuid:" ++ renderUUID uuid
  UrnDevice dom ty ver ->
    "urn:" ++ dom ++ ":device:" ++ ty ++ ":" ++ ver
  UrnService dom ty ver ->
    "urn:" ++ dom ++ ":service:" ++ ty ++ ":" ++ ver
