{-# LANGUAGE MagicHash, ForeignFunctionInterface, UnliftedFFITypes #-}
-- | This module contains a function which validates the content of a 'Text'
-- value. Note that the content of a 'Text' value should /always/ be valid (but
-- it doesn't hurt to test it).
--
module Data.Text.Tests.Validate
    (
      validate
    ) where

import Data.Text.Array (Array(aBA))
import Data.Text.Internal (Text (..))
import Foreign.C (CInt)
import GHC.Base (ByteArray#)
import System.IO.Unsafe (unsafePerformIO)

-- | Validate the bytes of a 'Text' value.
validate :: Text -> Bool
validate (Text arr off len) = unsafePerformIO $ do
    e <- hs_utf8_validate (aBA arr) (fromIntegral off) (fromIntegral len)
    return $! fromIntegral e == len

-- | We import this function from the @cbits/utf8_validate.c@ file
foreign import ccall unsafe "_hs_utf8_validate" hs_utf8_validate
    :: ByteArray# -> CInt -> CInt -> IO CInt
