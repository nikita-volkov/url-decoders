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


data QueryByte =
  DecodedQueryByte !Word8 | SpecialQueryByte !Word8

{-# NOINLINE query #-}
query :: BinaryParser (A.HashMap Text [Text])
query =
  recur A.empty
  where
    recur map =
      accumulateKey mempty
      where
        accumulateKey accumulator =
          optional queryByte >>= \case
            Just x -> case x of
              DecodedQueryByte byte -> addByte byte
              SpecialQueryByte byte -> case byte of
                61 -> accumulateValue key mempty
                38 -> recur (updatedMap key [])
                91 -> finalizeArrayDeclaration <|> failure ("Broken array declaration at key \"" <> key <> "\"")
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
                  63 -> recur (updatedMap key [])
                  x -> failure ("Unexpected byte: " <> (fromString . show) x)
            key =
              E.decodeUtf8With F.lenientDecode (G.toByteString accumulator)
        accumulateValue key accumulator =
          optional queryByte >>= \case
            Just x -> case x of
              DecodedQueryByte byte -> appendDecodedChar byte
              SpecialQueryByte byte -> case byte of
                38 -> recur (updatedMap key [value])
                _ -> appendDecodedChar byte
            Nothing -> return (updatedMap key [value])
          where
            appendDecodedChar byte =
              accumulateValue key (accumulator <> G.byte byte)
            value =
              E.decodeUtf8With F.lenientDecode (G.toByteString accumulator)
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
    37 -> possiblePercentEncodedByteBody (\_ _ -> failure "Broken percent encoding") decoded
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
      37 -> possiblePercentEncodedByteBody (\_ _ -> failure "Broken percent encoding") (return . DecodedQueryByte)
      43 -> return (DecodedQueryByte 32)
      38 -> return (SpecialQueryByte 38)
      59 -> return (SpecialQueryByte 38)
      61 -> return (SpecialQueryByte 61)
      91 -> return (SpecialQueryByte 91)
      93 -> return (SpecialQueryByte 93)
      35 -> failure ("Invalid query character: \"#\"")
      63 -> failure ("Invalid query character: \"?\"")
      _ -> return (DecodedQueryByte firstByte)

{-# INLINE possiblePercentEncodedByteBody #-}
possiblePercentEncodedByteBody :: (Word8 -> Word8 -> BinaryParser a) -> (Word8 -> BinaryParser a) -> BinaryParser a
possiblePercentEncodedByteBody failed succeeded =
  possibleHexByte failedFirstByte succeededFirstByte
  where
    failedFirstByte byte1 =
      do
        byte2 <- byte
        failed byte1 byte2
    succeededFirstByte byte1 =
      possibleHexByte failedSecondByte succeededSecondByte
      where
        failedSecondByte byte2 =
          failed byte1 byte2
        succeededSecondByte byte2 =
          succeeded (shiftL byte1 4 .|. byte2)

{-# INLINE possibleHexByte #-}
possibleHexByte :: (Word8 -> BinaryParser a) -> (Word8 -> BinaryParser a) -> BinaryParser a
possibleHexByte failed succeeded =
  do
    x <- byte
    if x >= 48 && x <= 57
      then succeeded (x - 48)
      else if x >= 65 && x <= 70
        then succeeded (x - 55)
        else if x >= 97 && x <= 102
          then succeeded (x - 87)
          else failed x

{-# INLINE byteWhichIs #-}
byteWhichIs :: Word8 -> BinaryParser ()
byteWhichIs expected =
  do
    actual <- byte
    unless (actual == expected) (failure ("Byte " <> (fromString . show) actual <> " doesn't equal the expected " <> (fromString . show) expected))
