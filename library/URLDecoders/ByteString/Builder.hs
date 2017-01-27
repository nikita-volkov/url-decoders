module URLDecoders.ByteString.Builder where

import BasePrelude
import qualified Data.ByteString as A
import qualified Data.ByteString.Internal as B
import qualified Foreign as D


data Builder =
  Builder !(D.Ptr Word8 -> IO (D.Ptr Word8)) !Int

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty =
    Builder return 0
  {-# INLINE mappend #-}
  mappend (Builder leftAction leftSize) (Builder rightAction rightSize) =
    Builder action size
    where
      action pointer =
        leftAction pointer >>= rightAction
      size =
        leftSize + rightSize

{-# INLINE byte #-}
byte :: Word8 -> Builder
byte x =
  Builder action 1
  where
    action pointer =
      D.poke pointer x $> D.plusPtr pointer 1

{-# INLINE toByteString #-}
toByteString :: Builder -> A.ByteString
toByteString (Builder action size) =
  B.unsafeCreate size (void . action)

{-# INLINE toLength #-}
toLength :: Builder -> Int
toLength (Builder _ size) =
  size
