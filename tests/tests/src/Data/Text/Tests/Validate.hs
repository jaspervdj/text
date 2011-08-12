{-# LANGUAGE MagicHash, ForeignFunctionInterface, UnliftedFFITypes #-}
-- | This module contains a function which validates the content of a 'Text'
-- value. Note that the content of a 'Text' value should /always/ be valid (but
-- it doesn't hurt to test it).
--
module Data.Text.Tests.Validate
    (
      validate
    , validateLazy
    ) where

import Data.Text.Array (Array(aBA))
import Foreign.C (CInt)
import GHC.Base (ByteArray#)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy.Internal as TL

-- | Validate the bytes of a 'Text' value.
validate :: T.Text -> Bool
validate (T.Text arr off len) = unsafePerformIO $ do
    e <- hs_text_utf8_validate (aBA arr) (fromIntegral off) (fromIntegral len)
    return $! fromIntegral e == len

-- | Variant of 'validate' for lazy text
validateLazy :: TL.Text -> Bool
validateLazy = TL.foldrChunks (\c -> (validate c &&)) True

-- | We import this function from the @cbits/utf8_validate.c@ file
foreign import ccall unsafe "_hs_text_utf8_validate" hs_text_utf8_validate
    :: ByteArray# -> CInt -> CInt -> IO CInt
