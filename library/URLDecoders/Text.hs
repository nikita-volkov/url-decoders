module URLDecoders.Text where

import BasePrelude
import qualified Data.Text.Internal.Fusion as A
import qualified URLDecoders.Text.Stream as B
import qualified Data.ByteString as C
import qualified Data.Text as D


urlEncodedBytes :: C.ByteString -> D.Text 
urlEncodedBytes bytes =
  A.unstream (B.urlEncodedBytes bytes)
