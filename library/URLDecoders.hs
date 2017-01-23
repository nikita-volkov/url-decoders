module URLDecoders where

import BasePrelude
import qualified URLDecoders.Text as A
import qualified Data.ByteString as B
import qualified Data.Text as C


{-|
Decoder of URL-encoded bytes (possibly with UTF8) into Text.
-}
utf8 :: B.ByteString -> C.Text
utf8 =
  A.urlEncodedBytes
