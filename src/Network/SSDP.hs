{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.SSDP
  ( -- * SSDP related types
    SSDP, Search, Notify
  , ST (..)
  , Header (..), HasHeader (..)
  , UserAgent, MX, MaxAge, Location, Server, BootId, ConfigId, Searchport
  , UUID, generateUUID, mkUUID
    -- * SSDP messages
  , ssdpSearch, ssdpSearchResponse
    -- * Sending & receiving SSDP messages
  , sendNotify, sendSearch
  , Renderable (..)
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception    as E
import           Control.Monad
import           Control.Monad.Trans

import           Data.Maybe

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
  -> (S.SockAddr -> SSDP Notify -> IO a) -- ^ callback for replies
  -> m [a]
sendSearch ssdp callback = liftIO $ do

  (sock, sockaddr) <- multicastSender ssdpAddr ssdpPort
  _ <- S.sendTo sock (render ssdp) sockaddr

  results <- newTChanIO
  count   <- newTVarIO (0 :: Int)

  let mx = get MXH ssdp

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

             Left err     -> do -- FIXME
               putStrLn $ show err ++ ":\n"
               mapM_ print $ lines msg
               putStrLn "\n"

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

  chanToList chan = do
    isempty <- isEmptyTChan chan
    if isempty
       then return []
       else (:) <$> readTChan chan
                <*> chanToList chan



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
