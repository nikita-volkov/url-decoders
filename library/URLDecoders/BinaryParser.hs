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
import qualified URLDecoders.ByteString.Builder as G
import qualified URLDecoders.PercentEncoding as H


data QueryByte =
  DecodedQueryByte !Word8 | SpecialQueryByte !Word8

{-# INLINE utf8Query #-}
utf8Query :: BinaryParser (A.HashMap Text [Text])
utf8Query =
  accumulateEntries A.empty
  where
    accumulateEntries map =
      accumulateKey mempty
      where
        accumulateKey accumulator =
          optional queryByte >>= \case
            Just x -> case x of
              DecodedQueryByte byte -> addByte byte
              SpecialQueryByte byte -> case byte of
                61 -> decodeKey >>= \key -> accumulateValue key mempty
                38 -> decodeKey >>= \key -> accumulateEntries (updatedMap key [])
                91 -> finalizeArrayDeclaration <|> failure ("Broken array declaration")
                93 -> failure "Unexpected character: \"]\""
                _ -> addByte byte
            Nothing -> if G.toLength accumulator == 0
              then return map
              else decodeKey >>= \key -> return (updatedMap key [])
          where
            addByte byte =
              accumulateKey (accumulator <> G.byte byte)
            finalizeArrayDeclaration =
              do
                byteWhichIs 93
                byte >>= \case
                  61 -> decodeKey >>= \key -> accumulateValue key mempty
                  63 -> decodeKey >>= \key -> accumulateEntries (updatedMap key [])
                  x -> failure ("Unexpected byte: " <> (fromString . show) x)
            decodeKey =
              decodeUTF8 (G.toByteString accumulator)
        accumulateValue key accumulator =
          optional queryByte >>= \case
            Just x -> case x of
              DecodedQueryByte byte -> addByte byte
              SpecialQueryByte byte -> case byte of
                38 -> decodeValue >>= \value -> accumulateEntries (updatedMap key [value])
                _ -> addByte byte
            Nothing -> decodeValue >>= \value -> return (updatedMap key [value])
          where
            addByte byte =
              accumulateValue key (accumulator <> G.byte byte)
            decodeValue =
              decodeUTF8 (G.toByteString accumulator)
        updatedMap key value =
          A.insertWith (<>) key value map

{-# INLINE asciiQuery #-}
asciiQuery :: BinaryParser (A.HashMap ByteString [ByteString])
asciiQuery =
  accumulateEntries A.empty
  where
    accumulateEntries map =
      accumulateKey mempty
      where
        accumulateKey accumulator =
          optional queryByte >>= \case
            Just x -> case x of
              DecodedQueryByte byte -> addByte byte
              SpecialQueryByte byte -> case byte of
                61 -> accumulateValue key mempty
                38 -> accumulateEntries (updatedMap key [])
                91 -> finalizeArrayDeclaration <|> failure ("Broken array declaration")
                93 -> failure "Unexpected character: \"]\""
                _ -> addByte byte
            Nothing -> if G.toLength accumulator == 0
              then return map
              else return (updatedMap key [])
          where
            addByte byte =
              accumulateKey (accumulator <> G.byte byte)
            finalizeArrayDeclaration =
              do
                byteWhichIs 93
                byte >>= \case
                  61 -> accumulateValue key mempty
                  63 -> accumulateEntries (updatedMap key [])
                  x -> failure ("Unexpected byte: " <> (fromString . show) x)
            key =
              G.toByteString accumulator
        accumulateValue key accumulator =
          optional queryByte >>= \case
            Just x -> case x of
              DecodedQueryByte byte -> addByte byte
              SpecialQueryByte byte -> case byte of
                38 -> accumulateEntries (updatedMap key [value])
                _ -> addByte byte
            Nothing -> return (updatedMap key [value])
          where
            addByte byte =
              accumulateValue key (accumulator <> G.byte byte)
            value =
              G.toByteString accumulator
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
    37 -> percentEncodedByteBody >>= decoded
    43 -> decoded 32
    38 -> special 38
    59 -> special 38
    61 -> special 61
    91 -> special 91
    93 -> special 93
    35 -> failure ("Invalid query character: \"#\"")
    63 -> failure ("Invalid query character: \"?\"")
    x -> decoded x

{-# INLINE queryByte #-}
queryByte :: BinaryParser QueryByte
queryByte =
  do
    firstByte <- byte
    case firstByte of
      37 -> DecodedQueryByte <$> percentEncodedByteBody
      43 -> return (DecodedQueryByte 32)
      38 -> return (SpecialQueryByte 38)
      59 -> return (SpecialQueryByte 38)
      61 -> return (SpecialQueryByte 61)
      91 -> return (SpecialQueryByte 91)
      93 -> return (SpecialQueryByte 93)
      35 -> failure ("Invalid query character: \"#\"")
      63 -> failure ("Invalid query character: \"?\"")
      _ -> return (DecodedQueryByte firstByte)

{-# INLINE percentEncodedByteBody #-}
percentEncodedByteBody :: BinaryParser Word8
percentEncodedByteBody =
  do
    byte1 <- byte
    byte2 <- byte
    H.matchPercentEncodedBytes (failure "Broken percent encoding") return byte1 byte2

{-# INLINE byteWhichIs #-}
byteWhichIs :: Word8 -> BinaryParser ()
byteWhichIs expected =
  do
    actual <- byte
    unless (actual == expected) (failure ("Byte " <> (fromString . show) actual <> " doesn't equal the expected " <> (fromString . show) expected))
