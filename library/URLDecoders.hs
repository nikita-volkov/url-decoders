module URLDecoders where

import BasePrelude
import qualified Data.ByteString as B
import qualified Data.Text as C
import qualified Data.HashMap.Strict as D
import qualified BinaryParser as E
import qualified URLDecoders.BinaryParser as F


{-|
Decodes the query part of a URL (the one following the question mark) or
the content of type @application/x-www-form-urlencoded@.

Produces a hash map of lists of values, interpreting the keys ending with @[]@
as arrays, as well as the repititive keys.
-}
{-# INLINE asciiQuery #-}
asciiQuery :: B.ByteString -> Either C.Text (D.HashMap B.ByteString [B.ByteString])
asciiQuery =
  E.run F.asciiQuery

{-|
Decodes the query part of a URL (the one following the question mark) or
the content of type @application/x-www-form-urlencoded@,
immediately applying a UTF8-decoding to it.

Produces a hash map of lists of values, interpreting the keys ending with @[]@
as arrays, as well as the repititive keys.
-}
{-# INLINE utf8Query #-}
utf8Query :: B.ByteString -> Either C.Text (D.HashMap C.Text [C.Text])
utf8Query =
  E.run F.utf8Query
