module URLDecoders.BinaryParser where

import BasePrelude
import BinaryParser
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as A
import qualified Data.ByteString as C
import qualified Data.Text as D
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as F
import qualified URLDecoders.ByteString as G


data QueryChunk a =
  DecodedQueryChunk !a | SpecialQueryChunk !a
  deriving (Functor, Show)

{-# NOINLINE query #-}
query :: BinaryParser (A.HashMap Text [Text])
query =
  recur A.empty
  where
    recur map =
      accumulateKey 0 []
      where
        accumulateKey length bytes =
          optional byteQueryChunk >>= \case
            Just x -> case x of
              DecodedQueryChunk byte -> addByte byte
              SpecialQueryChunk byte -> case byte of
                61 -> accumulateValue key 0 []
                38 -> recur (updatedMap key [])
                91 -> finalizeArrayDeclaration <|> failure ("Broken array declaration at key \"" <> key <> "\"")
                93 -> failure "Unexpected character: \"]\""
                _ -> addByte byte
            Nothing -> if length == 0
              then return map
              else return (updatedMap key [])
          where
            addByte byte =
              accumulateKey (succ length) (byte : bytes)
            finalizeArrayDeclaration =
              do
                byteWhichIs 93
                byte >>= \case
                  61 -> accumulateValue key 0 []
                  63 -> recur (updatedMap key [])
                  x -> failure ("Unexpected byte: " <> (fromString . show) x)
            key =
              E.decodeUtf8With F.lenientDecode (G.packReverseBytesWithLength length bytes)
        accumulateValue key length bytes =
          optional byteQueryChunk >>= \case
            Just x -> case x of
              DecodedQueryChunk byte -> appendDecodedChar byte
              SpecialQueryChunk byte -> case byte of
                38 -> recur (updatedMap key [value])
                _ -> appendDecodedChar byte
            Nothing -> return (updatedMap key [value])
          where
            appendDecodedChar byte =
              accumulateValue key (succ length) (byte : bytes)
            value =
              E.decodeUtf8With F.lenientDecode (G.packReverseBytesWithLength length bytes)
        updatedMap key value =
          A.insertWith (<>) key value map

{-# INLINE decodeUTF8 #-}
decodeUTF8 :: ByteString -> BinaryParser Text
decodeUTF8 bytes =
  case E.decodeUtf8' bytes of
    Left _ -> failure "Broken UTF8 sequence"
    Right text -> return text

{-# INLINE specialOrDecodedByte #-}
specialOrDecodedByte :: (Word8 -> BinaryParser a) -> (Word8 -> BinaryParser a) -> BinaryParser a
specialOrDecodedByte special decoded =
  byte >>= \case
    37 -> (percentEncodedByteBody >>= decoded) <|> special 37
    43 -> decoded 32
    38 -> special 38
    59 -> special 38
    61 -> special 61
    91 -> special 91
    93 -> special 93
    35 -> failure ("Invalid query character: \"#\"")
    63 -> failure ("Invalid query character: \"?\"")
    x -> decoded x

{-# INLINE byteQueryChunk #-}
byteQueryChunk :: BinaryParser (QueryChunk Word8)
byteQueryChunk =
  do
    firstByte <- byte
    case firstByte of
      37 -> DecodedQueryChunk <$> percentEncodedByteBody <|> return (SpecialQueryChunk 37)
      43 -> return (DecodedQueryChunk 32)
      38 -> return (SpecialQueryChunk 38)
      59 -> return (SpecialQueryChunk 38)
      61 -> return (SpecialQueryChunk 61)
      91 -> return (SpecialQueryChunk 91)
      93 -> return (SpecialQueryChunk 93)
      35 -> failure ("Invalid query character: \"#\"")
      63 -> failure ("Invalid query character: \"?\"")
      _ -> return (DecodedQueryChunk firstByte)

{-# INLINE percentEncodedByteBody #-}
percentEncodedByteBody :: BinaryParser Word8
percentEncodedByteBody =
  combine <$> hexByte <*> hexByte
  where
    combine l r =
      shiftL l 4 .|. r

{-# INLINE hexByte #-}
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

{-# INLINE byteWhichIs #-}
byteWhichIs :: Word8 -> BinaryParser ()
byteWhichIs expected =
  do
    actual <- byte
    unless (actual == expected) (failure ("Byte " <> (fromString . show) actual <> " doesn't equal the expected " <> (fromString . show) expected))
