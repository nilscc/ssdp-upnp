{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.SSDP.Types where

import Data.String

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

class HasHeader ssdp header where
  get :: ssdp -> header

infixr 0 :-
infixr 0 :?

newtype Host = Host String
  deriving (Eq, Show, IsString)

newtype Location = Location String
  deriving (Eq, Show, IsString)

newtype Server = Server String
  deriving (Eq, Show, IsString)

-- | @CACHE-CONTROL: max-age=@ header
newtype MaxAge = MaxAge Int
  deriving (Eq, Show, Num)

newtype UserAgent = UserAgent String
  deriving (Eq, Show, IsString)

newtype EXT = EXT String
  deriving (Eq, Show, IsString)

newtype Man = Man String
  deriving (Eq, Show, IsString)

newtype MX = MX Int
  deriving (Eq, Show, Num)

newtype BootId = BootId Int
  deriving (Eq, Show, Num)

newtype ConfigId = ConfigId Int
  deriving (Eq, Show, Num)

newtype Searchport = Searchport Int
  deriving (Eq, Show, Num)

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
