module URLDecoders.BinaryParser where

import BasePrelude
import BinaryParser
import Data.Text (Text)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as A
import qualified Data.ByteString as C
import qualified Data.Text as D
import qualified URLDecoders.UTF8CharDecoder as E
import qualified Text.Builder.Char as F
import qualified Text.Builder.StrictText as G


newtype Key =
  Key Text
  deriving (Eq, Hashable)

newtype Value =
  Value Text

newtype Query =
  Query (A.HashMap Key [Value])

data QueryChunk a =
  DecodedQueryChunk !a | SpecialQueryChunk !Char
  deriving (Functor, Show)

query :: BinaryParser Query
query =
  fmap Query (process A.empty)
  where
    process map =
      accumulateKey mempty
      where
        accumulateKey accumulator =
          optional charQueryChunk >>= \case
            Just x -> case x of
              DecodedQueryChunk char -> appendChar char
              SpecialQueryChunk char -> case char of
                '+' -> appendChar ' '
                '%' -> appendChar '%'
                '[' -> finalizeArrayDeclaration <|> failure ("Broken array declaration at key \"" <> key <> "\"")
                '=' -> accumulateValue key mempty
                '&' -> updateMap key [] True
                ']' -> failure "Unexpected character: \"]\""
                _ -> appendChar char
            Nothing -> if D.null key
              then return map
              else updateMap key [] False
          where
            appendChar char =
              accumulateKey (accumulator <> F.char char)
            finalizeArrayDeclaration =
              do
                byteWhichIs 93
                byte >>= \case
                  61 -> accumulateValue key mempty
                  63 -> updateMap key [] True
                  x -> failure ("Unexpected byte: " <> (fromString . show) x)
            key =
              G.charBuilder accumulator
        accumulateValue key accumulator =
          optional charQueryChunk >>= \case
            Just x -> case x of
              DecodedQueryChunk char -> appendChar char
              SpecialQueryChunk char -> case char of
                '+' -> appendChar ' '
                '%' -> appendChar '%'
                '&' -> updateMap key [value] True
                _ -> appendChar char
            Nothing -> updateMap key [value] False
          where
            appendChar char =
              accumulateValue key (accumulator <> F.char char)
            value =
              G.charBuilder accumulator
        updateMap key value continue =
          if continue
            then process (A.insertWith (<>) (Key key) (fmap Value value) map)
            else return map

byteQueryChunk :: BinaryParser (QueryChunk Word8)
byteQueryChunk =
  do
    firstByte <- byte
    case firstByte of
      37 -> DecodedQueryChunk <$> percentEncodedByteBody <|> return (SpecialQueryChunk '%')
      43 -> return (SpecialQueryChunk '+')
      63 -> return (SpecialQueryChunk '&')
      61 -> return (SpecialQueryChunk '=')
      91 -> return (SpecialQueryChunk '[')
      93 -> return (SpecialQueryChunk ']')
      _ -> return (DecodedQueryChunk firstByte)

charQueryChunk :: BinaryParser (QueryChunk Char)
charQueryChunk =
  byteQueryChunk >>= \case
    DecodedQueryChunk x -> DecodedQueryChunk <$> interpretedUTF8CharDecoderWithByte E.decodeByte x
    SpecialQueryChunk x -> return (SpecialQueryChunk x)

interpretedUTF8CharDecoderWithByte :: E.Decoder -> Word8 -> BinaryParser Char
interpretedUTF8CharDecoderWithByte decoder x =
  case decoder x of
    E.Unfinished nextDecoder ->
      do
        nextQueryByte <- byteQueryChunk <|> failure "Not enough bytes for a UTF8 sequence"
        case nextQueryByte of
          DecodedQueryChunk nextByte ->
            interpretedUTF8CharDecoderWithByte nextDecoder nextByte
          SpecialQueryChunk x ->
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
