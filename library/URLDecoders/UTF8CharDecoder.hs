module URLDecoders.UTF8CharDecoder where

import BasePrelude
import Data.Text (Text)
import qualified Data.Text.Internal.Encoding.Utf8 as A
import qualified Data.Text.Internal.Unsafe.Char as B


data Decoding =
  Unfinished !Decoder |
  Finished !Char |
  Failed !Word8 !Word8 !Word8 !Word8

type Decoder =
  Word8 -> Decoding

{-# INLINE decodeByte #-}
decodeByte :: Word8 -> Decoding
decodeByte byte1 =
  if A.validate1 byte1
    then
      Finished (B.unsafeChr8 byte1)
    else
      Unfinished $ \byte2 ->
        if A.validate2 byte1 byte2
          then
            Finished (A.chr2 byte1 byte2)
          else
            Unfinished $ \byte3 ->
              if A.validate3 byte1 byte2 byte3
                then
                  Finished (A.chr3 byte1 byte2 byte3)
                else
                  Unfinished $ \byte4 ->
                    if A.validate4 byte1 byte2 byte3 byte4
                      then
                        Finished (A.chr4 byte1 byte2 byte3 byte4)
                      else
                        Failed byte1 byte2 byte3 byte4
