module URLDecoders.Text.Stream where

import BasePrelude
import Data.Text.Internal.Fusion
import Data.Text.Internal.Fusion.Size
import qualified Data.Text.Internal.Encoding.Utf8 as A
import qualified Data.Text.Internal.Unsafe.Char as B
import qualified Data.ByteString as C
import qualified Data.ByteString.Unsafe as D


{-# INLINE urlEncodedBytes #-}
urlEncodedBytes :: C.ByteString -> Stream Char
urlEncodedBytes bytes =
  Stream next 0 (maxSize totalLength)
  where
    totalLength =
      C.length bytes
    next i
      | i >= l =
        Done
      | b1 == 37, l > 2, Just int1 <- hex b2, Just int2 <- hex b3 =
        Yield (B.unsafeChr8 (shiftL int1 4 .|. int2)) (i + 2)
      | A.validate1 b1 = 
        Yield (B.unsafeChr8 b1) (i + 1)
      | l > 1, A.validate2 b1 b2 = 
        Yield (A.chr2 b1 b2) (i + 2)
      | l > 2, A.validate3 b1 b2 b3 = 
        Yield (A.chr3 b1 b2 b3) (i + 3)
      | l > 3, A.validate4 b1 b2 b3 b4 = 
        Yield (A.chr4 b1 b2 b3 b4) (i + 4)
      | otherwise = 
        Yield '\xfffd' (i + 1)
      where
        hex b
          | 48 <= b && b <= 57 =
            Just (b - 48)
          | 65 <= b && b <= 70 =
            Just (b - 55)
          | 97 <= b && b <= 102 =
            Just (b - 87)
          | otherwise =
            Nothing
        l = 
          totalLength - i
        b1 = 
          idx i
        b2 = 
          idx (i + 1)
        b3 = 
          idx (i + 2)
        b4 = 
          idx (i + 3)
        idx = 
          D.unsafeIndex bytes



