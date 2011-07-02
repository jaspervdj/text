{-# LANGUAGE CPP, MagicHash, BangPatterns, ForeignFunctionInterface #-}

-- |
-- Module      : Data.Text.Encoding.Utf8
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- Basic UTF-8 validation and character manipulation.
module Data.Text.Encoding.Utf8
    (
    -- Decomposition
      ord2
    , ord3
    , ord4
    , charTailBytes
    -- Construction
    , chr2
    , chr3
    , chr4
    , chrUtf8
    -- * Validation
    , validate1
    , validate2
    , validate3
    , validate4
    , validateBS
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.Bits ((.&.))
import Data.ByteString.Internal (ByteString (..))
import Data.Text.UnsafeChar (ord, unsafeChr8)
import Data.Text.UnsafeShift (shiftR)
import Foreign.C (CInt)
import Foreign.Ptr ()
import Foreign (Ptr, withForeignPtr)
import GHC.Exts
import GHC.Word (Word8(..))
import System.IO.Unsafe (unsafePerformIO)

default(Int)

between :: Word8                -- ^ byte to check
        -> Word8                -- ^ lower bound
        -> Word8                -- ^ upper bound
        -> Bool
between x y z = x >= y && x <= z
{-# INLINE between #-}

ord2 :: Char -> (Word8,Word8)
ord2 c =
#if defined(ASSERTS)
    assert (n >= 0x80 && n <= 0x07ff)
#endif
    (x1,x2)
    where
      n  = ord c
      x1 = fromIntegral $ (n `shiftR` 6) + 0xC0
      x2 = fromIntegral $ (n .&. 0x3F)   + 0x80

ord3 :: Char -> (Word8,Word8,Word8)
ord3 c =
#if defined(ASSERTS)
    assert (n >= 0x0800 && n <= 0xffff)
#endif
    (x1,x2,x3)
    where
      n  = ord c
      x1 = fromIntegral $ (n `shiftR` 12) + 0xE0
      x2 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x3 = fromIntegral $ (n .&. 0x3F) + 0x80

ord4 :: Char -> (Word8,Word8,Word8,Word8)
ord4 c =
#if defined(ASSERTS)
    assert (n >= 0x10000)
#endif
    (x1,x2,x3,x4)
    where
      n  = ord c
      x1 = fromIntegral $ (n `shiftR` 18) + 0xF0
      x2 = fromIntegral $ ((n `shiftR` 12) .&. 0x3F) + 0x80
      x3 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x4 = fromIntegral $ (n .&. 0x3F) + 0x80

-- | Count the number of UTF-8 tail bytes needed to encode a character
charTailBytes :: Char -> Int
charTailBytes x
    | n < 0x00080 = 0
    | n < 0x00800 = 1
    | n < 0x10000 = 2
    | otherwise   = 3
  where
    n = ord x
{-# INLINE [0] charTailBytes #-}

chr2 :: Word8 -> Word8 -> Char
chr2 (W8# x1#) (W8# x2#) = C# (chr# (z1# +# z2#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
      !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3 :: Word8 -> Word8 -> Word8 -> Char
chr3 (W8# x1#) (W8# x2#) (W8# x3#) = C# (chr# (z1# +# z2# +# z3#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !y3# = word2Int# x3#
      !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
      !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4             :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
    C# (chr# (z1# +# z2# +# z3# +# z4#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !y3# = word2Int# x3#
      !y4# = word2Int# x4#
      !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
      !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
      !z4# = y4# -# 0x80#
{-# INLINE chr4 #-}

-- | Hybrid combination of 'unsafeChr8', 'chr2', 'chr3' and 'chr4'. This
-- function will not touch the bytes it doesn't need.
chrUtf8 :: (Char -> Int -> a) -> Word8 -> Word8 -> Word8 -> Word8 -> a
chrUtf8 f n1 n2 n3 n4
    | n1 < 0xC0 = f (unsafeChr8 n1)    1
    | n1 < 0xE0 = f (chr2 n1 n2)       2
    | n1 < 0xF0 = f (chr3 n1 n2 n3)    3
    | otherwise = f (chr4 n1 n2 n3 n4) 4
{-# INLINE [0] chrUtf8 #-}

validate1 :: Word8 -> Bool
validate1 x1 = x1 <= 0x7F
{-# INLINE validate1 #-}

validate2 :: Word8 -> Word8 -> Bool
validate2 x1 x2 = between x1 0xC2 0xDF && between x2 0x80 0xBF
{-# INLINE validate2 #-}

validate3 :: Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate3 #-}
validate3 x1 x2 x3 = validate3_1 || validate3_2 || validate3_3 || validate3_4
  where
    validate3_1 = (x1 == 0xE0) &&
                  between x2 0xA0 0xBF &&
                  between x3 0x80 0xBF
    validate3_2 = between x1 0xE1 0xEC &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF
    validate3_3 = x1 == 0xED &&
                  between x2 0x80 0x9F &&
                  between x3 0x80 0xBF
    validate3_4 = between x1 0xEE 0xEF &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF

validate4 :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate4 #-}
validate4 x1 x2 x3 x4 = validate4_1 || validate4_2 || validate4_3
  where 
    validate4_1 = x1 == 0xF0 &&
                  between x2 0x90 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_2 = between x1 0xF1 0xF3 &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_3 = x1 == 0xF4 &&
                  between x2 0x80 0x8F &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF

validateBS :: ByteString -> Int -> Int
validateBS (PS fptr _ l) o = unsafePerformIO $ withForeignPtr fptr $ \ptr -> do
    e <- hs_utf8_validate ptr (fromIntegral o) (fromIntegral l)
    return (fromIntegral e)

foreign import ccall unsafe "_hs_utf8_validate" hs_utf8_validate
    :: Ptr Word8 -> CInt -> CInt -> IO CInt
