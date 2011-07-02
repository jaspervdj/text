{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Text.Encoding
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
--
-- Functions for converting 'Text' values to and from 'ByteString',
-- using several standard encodings.
--
-- To gain access to a much larger family of encodings, use the
-- @text-icu@ package: <http://hackage.haskell.org/package/text-icu>

module Data.Text.Encoding
    (
    -- * Decoding ByteStrings to Text
    -- $strict
      decodeASCII
    , decodeUtf8
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE

    -- ** Catchable failure
    , decodeUtf8'

    -- ** Controllable error handling
    , decodeUtf8With
    , decodeUtf16LEWith
    , decodeUtf16BEWith
    , decodeUtf32LEWith
    , decodeUtf32BEWith

    -- * Encoding Text to ByteStrings
    , encodeUtf8
    , encodeUtf16LE
    , encodeUtf16BE
    , encodeUtf32LE
    , encodeUtf32BE
    ) where

import Control.Exception (evaluate, try)
import Control.Monad.ST (unsafeIOToST)
import Data.Bits (shiftL)
import Data.ByteString as B
import Data.ByteString.Internal as B
import Data.ByteString.Unsafe as B
import Data.Text.Array (copyToPtr, copyFromPtr)
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException, strictDecode)
import Data.Text.Internal (Text(..), textP)
import Data.Text.UnsafeChar (unsafeWrite)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (poke)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.Array as A
import qualified Data.Text.Encoding.Fusion as E
import qualified Data.Text.Encoding.Utf8 as U8
import qualified Data.Text.Fusion as F

import Debug.Trace

-- $strict
--
-- All of the single-parameter functions for decoding bytestrings
-- encoded in one of the Unicode Transformation Formats (UTF) operate
-- in a /strict/ mode: each will throw an exception if given invalid
-- input.
--
-- Each function has a variant, whose name is suffixed with -'With',
-- that gives greater control over the handling of decoding errors.
-- For instance, 'decodeUtf8' will throw an exception, but
-- 'decodeUtf8With' allows the programmer to determine what to do on a
-- decoding error.

-- | Decode a 'ByteString' containing 7-bit ASCII encoded text.
decodeASCII :: ByteString -> Text
decodeASCII bs = F.unstream (E.streamASCII bs)
{-# INLINE decodeASCII #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
decodeUtf8With :: OnDecodeError -> ByteString -> Text
decodeUtf8With onErr bs = textP textA 0 textLen
  where
    PS fptr off len = bs
    
    -- Position of the first character "after" the bytestring
    end = off + len

    (textA, textLen) = A.run2 $ do
        a <- A.new len
        go a len 0 off

    -- Main decoding loop
    --
    -- * arr: the text array, and;
    --
    -- * arrLen: it's length;
    --
    -- * n: our current position in the text array;
    --
    -- * m: current position in the bytestring;
    --
    go arr arrLen n m
        -- Reached the end: copy what we have
        | m' >= end = do
            copyTo arr
            return (arr, n + copyLen)

        -- Not at the end: decoding error
        | otherwise   = case onErr desc (Just errByte) of
            -- No replace character, so just copy and move on
            Nothing -> do
                copyTo arr
                go arr arrLen (n + copyLen) (m' + 1)

            -- Copy, copy replace character with optional resize and continue
            Just c  -> do
                -- The needed len
                let needed = n + copyLen + U8.charTailBytes c + end - m' - 1
                if needed < arrLen
                    -- Long enough
                    then do
                        copyTo arr
                        w <- unsafeWrite arr (n + copyLen) c
                        go arr arrLen (n + copyLen + w) (m' + 1)

                    -- Resize needed
                    else do
                        let arrLen' = arrLen `shiftL` 1
                        arr' <- A.new arrLen'
                        A.copyM arr' 0 arr 0 n
                        copyTo arr'
                        w <- unsafeWrite arr' (n + copyLen) c
                        go arr' arrLen' (n + copyLen + w) (m' + 1)
      where
        -- Everything in the range [m, m'[ is valid
        m' = U8.validateBS bs m
        errByte = B.unsafeIndex bs m'

        -- Length of the valid piece
        copyLen = m' - m

        -- Copy everything in the [m, m'[ range to some array
        copyTo a = unsafeIOToST $ withForeignPtr fptr $ \ptr ->
            copyFromPtr a n ptr m (n + copyLen)

    -- A validate loop through the bytestring. Returns the first position at
    -- which an invalid byte is found
    validate !i
        | i >= end                 = end
        | U8.validate1 x1          = validate (i + 1)
        | U8.validate2 x1 x2       = validate (i + 2)
        | U8.validate3 x1 x2 x3    = validate (i + 3)
        | U8.validate4 x1 x2 x3 x4 = validate (i + 4)
        | otherwise                = i
      where
        x1 = idx i
        x2 = idx (i + 1)
        x3 = idx (i + 2)
        x4 = idx (i + 3)
        idx = B.unsafeIndex bs
    
    desc = "Data.Text.Encoding.decodeUtf8: Invalid UTF-8 stream"
{-# INLINE[0] decodeUtf8With #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text that is known
-- to be valid.
--
-- If the input contains any invalid UTF-8 data, an exception will be
-- thrown that cannot be caught in pure code.  For more control over
-- the handling of invalid data, use 'decodeUtf8'' or
-- 'decodeUtf8With'.
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With strictDecode
{-# INLINE[0] decodeUtf8 #-}
{-# RULES "STREAM stream/decodeUtf8 fusion" [1]
    forall bs. F.stream (decodeUtf8 bs) = E.streamUtf8 strictDecode bs #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text..
--
-- If the input contains any invalid UTF-8 data, the relevant
-- exception will be returned, otherwise the decoded text.
decodeUtf8' :: ByteString -> Either UnicodeException Text
decodeUtf8' = unsafePerformIO . try . evaluate . decodeUtf8With strictDecode
{-# INLINE decodeUtf8' #-}

-- | Encode text using UTF-8 encoding.
encodeUtf8 :: Text -> ByteString
encodeUtf8 (Text arr off len) = unsafePerformIO $ do
    fp <- mallocByteString len
    withForeignPtr fp $ \ptr -> copyToPtr ptr 0 arr off (off + len)
    return $! PS fp 0 len
{-# INLINE encodeUtf8 #-}

-- | Decode text from little endian UTF-16 encoding.
decodeUtf16LEWith :: OnDecodeError -> ByteString -> Text
decodeUtf16LEWith onErr bs = F.unstream (E.streamUtf16LE onErr bs)
{-# INLINE decodeUtf16LEWith #-}

-- | Decode text from little endian UTF-16 encoding.
--
-- If the input contains any invalid little endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16LEWith'.
decodeUtf16LE :: ByteString -> Text
decodeUtf16LE = decodeUtf16LEWith strictDecode
{-# INLINE decodeUtf16LE #-}

-- | Decode text from big endian UTF-16 encoding.
decodeUtf16BEWith :: OnDecodeError -> ByteString -> Text
decodeUtf16BEWith onErr bs = F.unstream (E.streamUtf16BE onErr bs)
{-# INLINE decodeUtf16BEWith #-}

-- | Decode text from big endian UTF-16 encoding.
--
-- If the input contains any invalid big endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16BEWith'.
decodeUtf16BE :: ByteString -> Text
decodeUtf16BE = decodeUtf16BEWith strictDecode
{-# INLINE decodeUtf16BE #-}

-- | Encode text using little endian UTF-16 encoding.
encodeUtf16LE :: Text -> ByteString
encodeUtf16LE txt = E.unstream (E.restreamUtf16LE (F.stream txt))
{-# INLINE encodeUtf16LE #-}

-- | Encode text using big endian UTF-16 encoding.
encodeUtf16BE :: Text -> ByteString
encodeUtf16BE txt = E.unstream (E.restreamUtf16BE (F.stream txt))
{-# INLINE encodeUtf16BE #-}

-- | Decode text from little endian UTF-32 encoding.
decodeUtf32LEWith :: OnDecodeError -> ByteString -> Text
decodeUtf32LEWith onErr bs = F.unstream (E.streamUtf32LE onErr bs)
{-# INLINE decodeUtf32LEWith #-}

-- | Decode text from little endian UTF-32 encoding.
--
-- If the input contains any invalid little endian UTF-32 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf32LEWith'.
decodeUtf32LE :: ByteString -> Text
decodeUtf32LE = decodeUtf32LEWith strictDecode
{-# INLINE decodeUtf32LE #-}

-- | Decode text from big endian UTF-32 encoding.
decodeUtf32BEWith :: OnDecodeError -> ByteString -> Text
decodeUtf32BEWith onErr bs = F.unstream (E.streamUtf32BE onErr bs)
{-# INLINE decodeUtf32BEWith #-}

-- | Decode text from big endian UTF-32 encoding.
--
-- If the input contains any invalid big endian UTF-32 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf32BEWith'.
decodeUtf32BE :: ByteString -> Text
decodeUtf32BE = decodeUtf32BEWith strictDecode
{-# INLINE decodeUtf32BE #-}

-- | Encode text using little endian UTF-32 encoding.
encodeUtf32LE :: Text -> ByteString
encodeUtf32LE txt = E.unstream (E.restreamUtf32LE (F.stream txt))
{-# INLINE encodeUtf32LE #-}

-- | Encode text using big endian UTF-32 encoding.
encodeUtf32BE :: Text -> ByteString
encodeUtf32BE txt = E.unstream (E.restreamUtf32BE (F.stream txt))
{-# INLINE encodeUtf32BE #-}
