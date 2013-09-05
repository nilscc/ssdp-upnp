module Network.SSDP.UUID where

import Network.SSDP.Types

generateUUID :: IO UUID
generateUUID = undefined

mkUUID :: (String, String, String, String, String) -> UUID
mkUUID = UUID
