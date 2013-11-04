{-# OPTIONS -fno-warn-orphans #-}

module Network.UPnP.Render
  ( renderArguments
  ) where

import qualified Data.ByteString.Char8  as B8
import           Text.XML.Light

import           Network.UPnP.Types
import           Network.SSDP.Render    ()
import           Network.SSDP.Types     hiding (ST(..))

for :: [a] -> (a -> b) -> [b]
for = flip map

name :: String -> QName
name n = QName n Nothing Nothing

prefName :: String  -- ^ Prefix
         -> String  -- ^ Name
         -> QName
prefName p n = QName { qName = n, qURI = Nothing, qPrefix = Just p }

sName, uName :: String -> QName
sName = prefName "s"
uName = prefName "u"

string :: String -> Content
string s = Text $ blank_cdata { cdData = s }

el :: QName -> [Attr] -> [Content] -> Content
el q a c = Elem $ Element q a c Nothing

instance Renderable UpnpValue where
  render val = case val of
    UpnpVal_ui1         v -> show v
    UpnpVal_ui2         v -> show v
    UpnpVal_ui4         v -> show v
    UpnpVal_i1          v -> show v
    UpnpVal_i2          v -> show v
    UpnpVal_i4          v -> show v
    UpnpVal_int         v -> show v
    UpnpVal_r4          v -> show v
    UpnpVal_r8          v -> show v
    UpnpVal_number      v -> show v
    UpnpVal_fixed_14_4  v -> show v
    UpnpVal_float       v -> show v
    UpnpVal_char        v -> [v]
    UpnpVal_string      v -> v
    UpnpVal_boolean     v -> show v
    UpnpVal_bin_base64  v -> B8.unpack v
    UpnpVal_bin_hex     v -> B8.unpack v
    UpnpVal_uri         v -> show v
    UpnpVal_uuid        v -> render v

renderArguments :: ServiceType -> ActionName -> [Argument] -> [Element]
renderArguments sty actionname args = onlyElems $
  parseXML "<?xml version=\"1.0\"?>"
  ++
  [ el ( sName "Envelope" )
       [ Attr (prefName "xmlns" "s")  "http://schemas.xmlsoap.org/soap/envelope/"
       , Attr (sName "encodingStyle") "http://schemas.xmlsoap.org/soap/encoding/" ]
       [ el (sName "body") []
            [ el ( uName actionname )
                 [ Attr (prefName "xmlns" "u")
                        ("urn:" ++ serviceVendorDomain sty ++ ":service:"
                                ++ serviceType sty ++ ":" ++ serviceVersion sty) ]
                 $ for args $ \arg ->
                   el (name (argumentName arg)) []
                      [string . render $ argumentValue arg]
            ]
       ]
  ]
