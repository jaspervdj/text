{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, MagicHash, Rank2Types,
    RecordWildCards, UnboxedTuples, UnliftedFFITypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- |
-- Module      : Data.Text.Array
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
--
-- Packed, unboxed, heap-resident arrays.  Suitable for performance
-- critical use, both in terms of large data quantities and high
-- speed.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.Text.Array as A
--
-- The names in this module resemble those in the 'Data.Array' family
-- of modules, but are shorter due to the assumption of qualifid
-- naming.
module Data.Text.Array
    (
    -- * Types
      Array(aBA)
    , MArray(maBA)

    -- * Functions
    , copyM
    , copyI
    , copyToPtr
    , copyFromPtr
    , empty
    , equal
    , diff
#if defined(ASSERTS)
    , length
#endif
    , run
    , run2
    , toList
    , unsafeFreeze
    , unsafeIndex
    , new
    , unsafeWrite
    ) where

#if defined(ASSERTS)
-- This fugly hack is brought by GHC's apparent reluctance to deal
-- with MagicHash and UnboxedTuples when inferring types. Eek!
# define CHECK_BOUNDS(_func_,_len_,_k_) \
if (_k_) < 0 || (_k_) >= (_len_) then error ("Data.Text.Array." ++ (_func_) ++ ": bounds error, offset " ++ show (_k_) ++ ", length " ++ show (_len_)) else
#else
# define CHECK_BOUNDS(_func_,_len_,_k_)
#endif

#include "MachDeps.h"

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Control.Monad.ST (unsafeIOToST)
import Data.Bits ((.&.), xor)
import Data.Text.Unsafe.Base (inlinePerformIO)
import Data.Text.UnsafeShift (shiftL, shiftR)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt, CSize)
import GHC.Base (ByteArray#, MutableByteArray#, Int(..),
                 indexWord8Array#, indexWordArray#, newByteArray#,
                 readWord8Array#, readWordArray#, unsafeCoerce#,
                 writeWord8Array#, writeWordArray#)
import GHC.ST (ST(..), runST)
import GHC.Word (Word8(..), Word(..))

import Prelude hiding (length, read)

-- | Immutable array type.
data Array = Array {
      aBA :: ByteArray#
#if defined(ASSERTS)
      -- TODO: We won't need this anymore
    , aLen :: {-# UNPACK #-} !Int -- length (in units of Word16, not bytes)
#endif
    }

-- | Mutable array type, for use in the ST monad.
data MArray s = MArray {
      maBA :: MutableByteArray# s
#if defined(ASSERTS)
      -- TODO: We won't need this anymore
    , maLen :: {-# UNPACK #-} !Int -- length (in units of Word16, not bytes)
#endif
    }

#if defined(ASSERTS)
-- | Operations supported by all arrays.
class IArray a where
    -- | Return the length of an array.
    length :: a -> Int

instance IArray Array where
    length = aLen
    {-# INLINE length #-}

instance IArray (MArray s) where
    length = maLen
    {-# INLINE length #-}
#endif

-- | Create an uninitialized mutable array.
new :: forall s. Int -> ST s (MArray s)
new n
  | n < 0 || n .&. highBit /= 0 = error $ "Data.Text.Array.new: size overflow"
  | otherwise = ST $ \s1# ->
       case newByteArray# len# s1# of
         (# s2#, marr# #) -> (# s2#, MArray marr#
#if defined(ASSERTS)
                                n
#endif
                                #)
  where !(I# len#) = bytesInArray n
        highBit    = maxBound `xor` (maxBound `shiftR` 1)
{-# INLINE new #-}

-- | Freeze a mutable array. Do not mutate the 'MArray' afterwards!
unsafeFreeze :: MArray s -> ST s Array
unsafeFreeze MArray{..} = ST $ \s# ->
                          (# s#, Array (unsafeCoerce# maBA)
#if defined(ASSERTS)
                             maLen
#endif
                             #)
{-# INLINE unsafeFreeze #-}

-- | Indicate how many bytes would be used for an array of the given
-- size.
bytesInArray :: Int -> Int
bytesInArray n = n `shiftL` 1
{-# INLINE bytesInArray #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex :: Array -> Int -> Word8
unsafeIndex Array{..} i@(I# i#) =
  CHECK_BOUNDS("unsafeIndex",aLen,i)
    case indexWord8Array# aBA i# of r# -> (W8# r#)
{-# INLINE unsafeIndex #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndexWord :: Array -> Int -> Word
unsafeIndexWord Array{..} i@(I# i#) =
  CHECK_BOUNDS("unsafeIndexWord",aLen`div`wordFactor,i)
    case indexWordArray# aBA i# of r# -> (W# r#)
{-# INLINE unsafeIndexWord #-}

-- | Unchecked read of a mutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeRead :: MArray s -> Int -> ST s Word8
unsafeRead MArray{..} i@(I# i#) = ST $ \s# ->
  CHECK_BOUNDS("unsafeRead",maLen,i)
  case readWord8Array# maBA i# s# of
    (# s2#, r# #) -> (# s2#, W8# r# #)
{-# INLINE unsafeRead #-}

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWrite :: MArray s -> Int -> Word8 -> ST s ()
unsafeWrite MArray{..} i@(I# i#) (W8# e#) = ST $ \s1# ->
  CHECK_BOUNDS("unsafeWrite",maLen,i)
  case writeWord8Array# maBA i# e# s1# of
    s2# -> (# s2#, () #)
{-# INLINE unsafeWrite #-}

-- | Convert an immutable array to a list.
toList :: Array -> Int -> Int -> [Word8]
toList ary off len = loop 0
    where loop i | i < len   = unsafeIndex ary (off+i) : loop (i+1)
                 | otherwise = []

-- | An empty immutable array.
empty :: Array
empty = runST (new 0 >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result.
run :: (forall s. ST s (MArray s)) -> Array
run k = runST (k >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result paired with whatever else the action returns.
run2 :: (forall s. ST s (MArray s, a)) -> (Array, a)
run2 k = runST (do
                 (marr,b) <- k
                 arr <- unsafeFreeze marr
                 return (arr,b))

-- | The amount to divide or multiply by to switch between units of
-- 'Word8' and units of 'Word'.
wordFactor :: Int
wordFactor = SIZEOF_HSWORD

-- | Indicate whether an offset is word-aligned.
wordAligned :: Int -> Bool
wordAligned i = i .&. (wordFactor - 1) == 0

-- | Copy some elements of a mutable array.
copyM :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> MArray s               -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ Count
      -> ST s ()
copyM dest didx src sidx count
    | count <= 0 = return ()
    | otherwise =
#if defined(ASSERTS)
    assert (sidx + count <= length src) .
    assert (didx + count <= length dest) .
#endif
    unsafeIOToST $ memcpyM (maBA dest) (fromIntegral didx)
                           (maBA src) (fromIntegral sidx)
                           (fromIntegral count)
{-# INLINE copyM #-}

-- | Copy some elements of an immutable array.
copyI :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> Array                  -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ First offset in destination /not/ to
                                -- copy (i.e. /not/ length)
      -> ST s ()
copyI dest i0 src j0 top
    | i0 >= top = return ()
    | otherwise = unsafeIOToST $
                  memcpyI (maBA dest) (fromIntegral i0)
                          (aBA src) (fromIntegral j0)
                          (fromIntegral (top-i0))
{-# INLINE copyI #-}

-- | Copy some elements of an immutable array to a pointer
copyToPtr :: Ptr Word8               -- ^ Destination
          -> Int                     -- ^ Destination offset
          -> Array                   -- ^ Source
          -> Int                     -- ^ Source offset
          -> Int                     -- ^ First offset in destination /not/ to
                                     -- copy (i.e. /not/ length)
          -> IO ()
copyToPtr dest i0 src j0 top
    | i0 >= top = return ()
    | otherwise = memcpyToPtr dest (fromIntegral i0)
                              (aBA src) (fromIntegral j0)
                              (fromIntegral (top - i0))
{-# INLINE copyToPtr #-}

-- | Copy some elements from a pointer to an array
copyFromPtr :: MArray s                -- ^ Destination
            -> Int                     -- ^ Destination offset
            -> Ptr Word8               -- ^ Source
            -> Int                     -- ^ Source offset
            -> Int                     -- ^ First offset in destination /not/ to
                                       -- copy (i.e. /not/ length)
            -> IO ()
copyFromPtr dest i0 src j0 top
    | i0 >= top = return ()
    | otherwise = memcpyFromPtr (maBA dest) (fromIntegral i0)
                                src (fromIntegral j0)
                                (fromIntegral (top - i0))
{-# INLINE copyFromPtr #-}

-- | Compare portions of two arrays for equality.  No bounds checking
-- is performed.
equal :: Array                  -- ^ First
      -> Int                    -- ^ Offset into first
      -> Array                  -- ^ Second
      -> Int                    -- ^ Offset into second
      -> Int                    -- ^ Count
      -> Bool
equal arrA offA arrB offB count = inlinePerformIO $ do
  i <- memcmp (aBA arrA) (fromIntegral offA)
                     (aBA arrB) (fromIntegral offB) (fromIntegral count)
  return $! i == 0
{-# INLINE equal #-}

-- | Compare portions of two arrays and return the offset in the first array
-- where a difference is found.
diff :: Array                  -- ^ First
     -> Int                    -- ^ Offset into first
     -> Array                  -- ^ Second
     -> Int                    -- ^ Offset into second
     -> Int                    -- ^ Count
     -> Int                    -- ^ Offset of first difference
diff arrA offA arrB offB count = inlinePerformIO $ do
  i <- c_diff (aBA arrA) (fromIntegral offA)
              (aBA arrB) (fromIntegral offB) (fromIntegral count)
  return $! fromIntegral i
{-# INLINE diff #-}

foreign import ccall unsafe "_hs_text_memcpy" memcpyI
    :: MutableByteArray# s -> CSize -> ByteArray# -> CSize -> CSize -> IO ()

foreign import ccall unsafe "_hs_text_memcpy" memcpyToPtr
    :: Ptr Word8 -> CSize -> ByteArray# -> CSize -> CSize -> IO ()

foreign import ccall unsafe "_hs_text_memcpy" memcpyFromPtr
    :: MutableByteArray# s -> CSize -> Ptr Word8 -> CSize -> CSize -> IO ()

foreign import ccall unsafe "_hs_text_memcmp" memcmp
    :: ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "_hs_text_memcpy" memcpyM
    :: MutableByteArray# s -> CSize -> MutableByteArray# s -> CSize -> CSize
    -> IO ()

foreign import ccall unsafe "_hs_text_diff" c_diff
    :: ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CSize
