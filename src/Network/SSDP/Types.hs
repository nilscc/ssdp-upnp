{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}

module Network.SSDP.Types where

data Search
data Notify
data Response

data SSDP a where
  SSDPSearch   :: MX -> String -> [Header] -> SSDP Search
  SSDPNotify   ::       String -> [Header] -> SSDP Notify
  SSDPResponse ::       String -> [Header] -> SSDP Response

data Header
  = String :- String
  | String :? Maybe String
  deriving Show

infixr 0 :-
infixr 0 :?

type UserAgent = String
type MX = Int

type MaxAge = Int
type Location = String
type Server = String

type BootId = Int
type ConfigId = Int
type Searchport = Int

newtype UUID = UUID (String, String, String, String, String)
  deriving (Show, Eq, Ord)

-- | SSDP Search targets
data ST
  = SsdpAll
  | UpnpRootDevice
  | UuidDevice UUID
  | UrnDevice  { deviceDomain  :: String
               , deviceType    :: String
               , deviceVersion :: String }
  | UrnService { serviceDomain  :: String
               , serviceType    :: String
               , serviceVersion :: String }
