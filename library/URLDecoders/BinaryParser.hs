module URLDecoders.BinaryParser where

import BasePrelude
import BinaryParser
import Data.Text (Text)
import qualified Data.HashMap.Strict as A
import qualified Data.ByteString as C
import qualified Data.Text as D
import qualified URLDecoders.UTF8CharDecoder as E
import qualified Text.Builder as F


data QueryChunk a =
  DecodedQueryChunk !a | SpecialQueryChunk !Char
  deriving (Functor, Show)

{-# INLINE query #-}
query :: BinaryParser (A.HashMap Text [Text])
query =
  recur A.empty
  where
    recur map =
      accumulateKey mempty
      where
        accumulateKey accumulator =
          optional charQueryChunk >>= \case
            Just x -> case x of
              DecodedQueryChunk char -> appendChar char
              SpecialQueryChunk char -> case char of
                '+' -> appendChar ' '
                '[' -> finalizeArrayDeclaration <|> failure ("Broken array declaration at key \"" <> key <> "\"")
                '=' -> accumulateValue key mempty
                '&' -> updateMapAndRecur key []
                ';' -> updateMapAndRecur key []
                ']' -> failure "Unexpected character: \"]\""
                _ -> appendChar char
            Nothing -> if D.null key
              then return map
              else updateMap key []
          where
            appendChar char =
              accumulateKey (accumulator <> F.char char)
            finalizeArrayDeclaration =
              do
                byteWhichIs 93
                byte >>= \case
                  61 -> accumulateValue key mempty
                  63 -> updateMapAndRecur key []
                  x -> failure ("Unexpected byte: " <> (fromString . show) x)
            key =
              F.run accumulator
        accumulateValue key accumulator =
          optional charQueryChunk >>= \case
            Just x -> case x of
              DecodedQueryChunk char -> appendChar char
              SpecialQueryChunk char -> case char of
                '+' -> appendChar ' '
                '&' -> updateMapAndRecur key [value]
                ';' -> updateMapAndRecur key [value]
                _ -> appendChar char
            Nothing -> updateMap key [value]
          where
            appendChar char =
              accumulateValue key (accumulator <> F.char char)
            value =
              F.run accumulator
        updateMapAndRecur key value =
          updateMap key value >>= recur
        updateMap key value =
          return (A.insertWith (<>) key value map)

{-# INLINE byteQueryChunk #-}
byteQueryChunk :: BinaryParser (QueryChunk Word8)
byteQueryChunk =
  do
    firstByte <- byte
    case firstByte of
      37 -> DecodedQueryChunk <$> percentEncodedByteBody <|> return (SpecialQueryChunk '%')
      43 -> return (SpecialQueryChunk '+')
      38 -> return (SpecialQueryChunk '&')
      59 -> return (SpecialQueryChunk ';')
      61 -> return (SpecialQueryChunk '=')
      91 -> return (SpecialQueryChunk '[')
      93 -> return (SpecialQueryChunk ']')
      35 -> failure ("Invalid query character: \"#\"")
      63 -> failure ("Invalid query character: \"?\"")
      _ -> return (DecodedQueryChunk firstByte)

{-# INLINE charQueryChunk #-}
charQueryChunk :: BinaryParser (QueryChunk Char)
charQueryChunk =
  byteQueryChunk >>= \case
    DecodedQueryChunk x -> do
      E.decodeCharMonadicallyHavingFirstByte x nextUTF8Byte >>= \case
        Just char -> return (DecodedQueryChunk char)
        Nothing -> failure "Invalid UTF8 byte sequence"
    SpecialQueryChunk x -> return (SpecialQueryChunk x)
  where
    nextUTF8Byte =
      do
        nextQueryByte <- byteQueryChunk <|> failure "Not enough bytes for a UTF8 sequence"
        case nextQueryByte of
          DecodedQueryChunk byte ->
            return byte
          SpecialQueryChunk x ->
            failure ("Unexpected special symbol: " <> (fromString . show) x)

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
