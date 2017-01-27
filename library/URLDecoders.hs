module URLDecoders where

import BasePrelude
import qualified Data.ByteString as B
import qualified Data.Text as C
import qualified Data.HashMap.Strict as D
import qualified BinaryParser as E
import qualified URLDecoders.BinaryParser as F

{-# INLINE query #-}
query :: B.ByteString -> Either C.Text (D.HashMap C.Text [C.Text])
query =
  E.run F.query
