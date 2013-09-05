module Network.SSDP.UUID where

newtype UUID = UUID String

renderUuid :: UUID -> String
renderUuid (UUID s) = s

generateUUID :: IO UUID
generateUUID = undefined

mkUUID :: String -> UUID
mkUUID = UUID
