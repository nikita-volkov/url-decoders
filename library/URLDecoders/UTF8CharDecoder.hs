module URLDecoders.UTF8CharDecoder where

import BasePrelude
import Data.Text (Text)
import qualified Data.Text.Internal.Encoding.Utf8 as A
import qualified Data.Text.Internal.Unsafe.Char as B


{-|
Given an effect, which provides the next byte, produces a decoded Char.
-}
{-# INLINE decodeCharMonadically #-}
decodeCharMonadically :: Monad m => m Word8 -> m (Maybe Char)
decodeCharMonadically getNextByte =
  do
    byte1 <- getNextByte
    decodeCharMonadicallyHavingFirstByte byte1 getNextByte

{-|
Given an effect, which provides the next byte, produces a decoded Char.
-}
{-# INLINE decodeCharMonadicallyHavingFirstByte #-}
decodeCharMonadicallyHavingFirstByte :: Monad m => Word8 -> m Word8 -> m (Maybe Char)
decodeCharMonadicallyHavingFirstByte byte1 getNextByte =
  if A.validate1 byte1
    then return (Just (B.unsafeChr8 byte1))
    else do
      byte2 <- getNextByte
      if A.validate2 byte1 byte2
        then return (Just (A.chr2 byte1 byte2))
        else do
          byte3 <- getNextByte
          if A.validate3 byte1 byte2 byte3
            then return (Just (A.chr3 byte1 byte2 byte3))
            else do
              byte4 <- getNextByte
              if A.validate4 byte1 byte2 byte3 byte4
                then return (Just (A.chr4 byte1 byte2 byte3 byte4))
                else return Nothing
