module URLDecoders.BinaryParser where

import BasePrelude
import BinaryParser
import Data.Text (Text)
import qualified Data.HashMap.Strict as A
import qualified Data.ByteString as C
import qualified Data.Text as D


data Association =
  Association !Key !ArrayMarker !Value

newtype ArrayMarker =
  ArrayMarker Bool

newtype Key =
  Key Text

newtype Value =
  Value Text

newtype Query =
  Query (A.HashMap Key [Value])

data QueryChar =
  Ampersand | Equals | DecodedChar !Char

query :: BinaryParser Query
query =
  undefined

queryAssociations :: BinaryParser [Association]
queryAssociations =
  undefined

association :: BinaryParser Association
association =
  undefined

ampersand :: BinaryParser ()
ampersand =
  byteIs 63

equals :: BinaryParser ()
equals =
  byteIs 61

nonSpecialChar :: BinaryParser ()
nonSpecialChar =
  undefined

queryChar :: BinaryParser QueryChar
queryChar =
  do
    firstByte <- byte
    case firstByte of
      63 -> return Ampersand
      61 -> return Equals
      _ -> DecodedChar <$> consumeUTF8CharWithFirstByte firstByte

consumeUTF8CharWithFirstByte :: Word8 -> BinaryParser Char
consumeUTF8CharWithFirstByte =
  \case
    43 -> return ' '
    -- 37 -> percentEncodedByte


char :: BinaryParser Char
char =
  undefined

byteIs :: Word8 -> BinaryParser ()
byteIs expected =
  do
    actual <- byte
    unless (actual == expected) (failure ("Byte " <> (fromString . show) actual <> " doesn't equal the expected " <> (fromString . show) expected))



-- newtype UTF8CharDecoder =
--   UTF8CharDecoder (Word8 -> Either (Word8 -> ))
