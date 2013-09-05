module Network.SSDP.UUID where

import Data.List
import Network.SSDP.Types

renderUUID :: UUID -> String
renderUUID (UUID (a,b,c,d,e)) = intercalate "-" [a,b,c,d,e]

generateUUID :: IO UUID
generateUUID = undefined

mkUUID :: (String, String, String, String, String) -> UUID
mkUUID = UUID
