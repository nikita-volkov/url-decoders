module URLDecoders.ByteString where

import BasePrelude
import qualified Data.ByteString as A
import qualified Data.ByteString.Internal as B
import qualified Foreign as D


{-# INLINE packReverseBytesWithLength #-}
packReverseBytesWithLength :: Int -> [Word8] -> A.ByteString
packReverseBytesWithLength length bytes =
  B.unsafeCreate length (updatePointer bytes)
  where
    updatePointer bytes pointer =
      case bytes of
        head : tail -> do
          D.poke pointer head
          updatePointer tail (D.plusPtr pointer (-1))
        _ -> return ()
