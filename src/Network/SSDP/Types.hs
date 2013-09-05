{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.SSDP.Types where

data Search
data Notify
data Response

data SSDP a = SSDP
  { ssdpStartingLine :: String
  , ssdpHeaders      :: [Header]
  }

data Header
  = String :- String
  | String :? Maybe String
  deriving Show

class Renderable a where
  render :: a -> String

class HasHeader ssdp header result where
  get :: header -> ssdp -> result

infixr 0 :-
infixr 0 :?

type MX = Int
type UserAgent = String
type MaxAge = Int
type Location = String
type Server = String
type BootId = Int
type ConfigId = Int
type Searchport = Int

data STH = STH
data HostH = HostH
data LocationH = LocationH
data ServerH = ServerH
data MaxAgeH = MaxAgeH
data UserAgentH = UserAgentH
data ExtH = EXTH
data ManH = ManH
data MXH = MXH
data BootIdH = BootIdH
data ConfigIdH = ConfigIdH
data SearchportH = SearchportH

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
  deriving (Show, Eq)

data USN = USN UUID ST
  deriving (Show, Eq)
