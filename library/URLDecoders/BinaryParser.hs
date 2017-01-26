module URLDecoders.BinaryParser where

import BasePrelude
import BinaryParser
import Data.Text (Text)
import qualified Data.HashMap.Strict as A
import qualified Data.ByteString as C
import qualified Data.Text as D
import qualified URLDecoders.UTF8CharDecoder as E


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

data QuerySymbol a =
  DecodedQuerySymbol !a | AmpersandQuerySymbol | EqualsQuerySymbol | PlusQuerySymbol
  deriving (Functor, Show)

query :: BinaryParser Query
query =
  undefined

queryAssociations :: BinaryParser [Association]
queryAssociations =
  undefined

association :: BinaryParser Association
association =
  undefined

queryByte :: BinaryParser (QuerySymbol Word8)
queryByte =
  do
    firstByte <- byte
    case firstByte of
      43 -> return PlusQuerySymbol
      63 -> return AmpersandQuerySymbol
      61 -> return EqualsQuerySymbol
      37 -> DecodedQuerySymbol <$> (percentEncodedByteBody <|> pure 37)
      _ -> return (DecodedQuerySymbol firstByte)

queryChar :: BinaryParser (QuerySymbol Char)
queryChar =
  queryByte >>= \case
    DecodedQuerySymbol x -> DecodedQuerySymbol <$> interpretedUTF8CharDecoderWithByte E.decodeByte x
    x -> return (unsafeCoerce x)

interpretedUTF8CharDecoderWithByte :: E.Decoder -> Word8 -> BinaryParser Char
interpretedUTF8CharDecoderWithByte decoder x =
  case decoder x of
    E.Unfinished nextDecoder ->
      do
        nextQueryByte <- queryByte <|> failure "Not enough bytes for a UTF8 sequence"
        case nextQueryByte of
          DecodedQuerySymbol nextByte ->
            interpretedUTF8CharDecoderWithByte nextDecoder nextByte
          x ->
            failure ("Unexpected special symbol: " <> (fromString . show) x)
    E.Finished char ->
      return char
    E.Failed byte1 byte2 byte3 byte4 ->
      failure ("Improper UTF8 byte sequence: " <> foldMap (fromString . show) [byte1, byte2, byte3, byte4])

percentEncodedByteBody :: BinaryParser Word8
percentEncodedByteBody =
  combine <$> hexByte <*> hexByte
  where
    combine l r =
      shiftL l 4 .|. r

hexByte :: BinaryParser Word8
hexByte =
  do
    x <- byte
    if x >= 48 && x <= 57
      then return (x - 48)
      else if x >= 65 && x <= 70
        then return (x - 55)
        else if x >= 97 && x <= 102
          then return (x - 87)
          else failure ("Not a hexadecimal byte: " <> (fromString . show) x)

byteWhichIs :: Word8 -> BinaryParser ()
byteWhichIs expected =
  do
    actual <- byte
    unless (actual == expected) (failure ("Byte " <> (fromString . show) actual <> " doesn't equal the expected " <> (fromString . show) expected))
