module Network.UPnP where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Text.XML.Light
import Network.HTTP

import Network.SSDP
import Network.UPnP.Types

--------------------------------------------------------------------------------
-- Device description

requestDeviceDescription :: SSDP Notify -> IO (Maybe DeviceDescription)
requestDeviceDescription ssdp = runMaybeT $ do
  loc <- require $ getHeaderValue "LOCATION" ssdp
  res <- liftIO $ simpleHTTP (getRequest loc)
  bdy <- rspBody <$> requireRight res
  return $ UpnpXml $ parseXML bdy
 where
  require = maybe (fail "Unexpected Nothing") return 
  requireRight = either (\_ -> fail "Unexpected Left") return
