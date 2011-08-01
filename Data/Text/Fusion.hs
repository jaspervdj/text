{-# LANGUAGE BangPatterns, MagicHash #-}

-- |
-- Module      : Data.Text.Fusion
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009-2010,
--               (c) Duncan Coutts 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- Text manipulation functions represented as fusible operations over
-- streams.
module Data.Text.Fusion
    (
    -- * Types
      Stream(..)
    , Step(..)

    -- * Creation and elimination
    , stream
    , unstream
    , reverseStream

    , length

    -- * Transformations
    , reverse

    -- * Construction
    -- ** Scans
    , reverseScanr

    -- ** Accumulating maps
    , mapAccumL

    -- ** Generation and unfolding
    , unfoldrN

    -- * Indexing
    , index
    , findIndex
    , countChar
    ) where

import Prelude (Bool(..), Char, Maybe(..), Monad(..), Int,
                Num(..), Ord(..), ($), (&&), otherwise)
import Data.Bits ((.&.))
import Data.Text.Internal (Text(..))
import Data.Text.UnsafeChar (ord, unsafeWrite)
import Data.Text.UnsafeShift (shiftL, shiftR)
import qualified Data.Text.Array as A
import qualified Data.Text.Fusion.Common as S
import Data.Text.Fusion.Internal
import Data.Text.Fusion.Size
import qualified Data.Text.Internal as I
import qualified Data.Text.Encoding.Utf8 as U8

default(Int)

-- | /O(n)/ Convert a 'Text' into a 'Stream Char'.
stream :: Text -> Stream Char
stream (Text arr off len) = Stream next off (maxSize len)
  where
    !end = off + len
    next !i
        | i >= end  = Done
        | otherwise = U8.decodeCharIndex (\c s -> Yield c (i + s)) idx i
      where
        idx = A.unsafeIndex arr
    {-# INLINE next #-}
{-# INLINE [0] stream #-}

-- | /O(n)/ Convert a 'Text' into a 'Stream Char', but iterate
-- backwards.
reverseStream :: Text -> Stream Char
reverseStream (Text arr off len) = Stream next (off+len-1) (maxSize len)
  where
    next !i
        | i < off                = Done
        -- We always have to guess and check if we're dealing with a
        -- continuation byte.
        | U8.continuationByte x1 = next (i - 1)
        | otherwise              =
            U8.decodeCharIndex (\c _ -> Yield c (i - 1)) idx i
      where
        x1 = idx i
        idx = A.unsafeIndex arr
    {-# INLINE next #-}
{-# INLINE [0] reverseStream #-}

-- | /O(n)/ Convert a 'Stream Char' into a 'Text'.
unstream :: Stream Char -> Text
unstream (Stream next0 s0 len) = I.textP textArr 0 textLen
  where
    (textArr, textLen) = A.run2 $ do
        let mlen = upperBound 4 len
        arr <- A.new mlen
        outer arr mlen s0 0

    outer arr top = loop
      where
        loop !s !i = case next0 s of
            Done            -> return (arr, i)
            Skip s'         -> loop s' i
            Yield x s'
                | j >= top  -> {-# SCC "unstream/resize" #-} do
                    let top' = (top + 1) `shiftL` 1
                    arr' <- A.new top'
                    A.copyM arr' 0 arr 0 top
                    outer arr' top' s i
                | otherwise -> do
                    d <- unsafeWrite arr i x
                    loop s' (i+d)
              where
                j = i + U8.charTailBytes x
{-# INLINE [0] unstream #-}
{-# RULES "STREAM stream/unstream fusion" forall s. stream (unstream s) = s #-}


-- ----------------------------------------------------------------------------
-- * Basic stream functions

length :: Stream Char -> Int
length = S.lengthI
{-# INLINE[0] length #-}

-- | /O(n)/ Reverse the characters of a string.
reverse :: Stream Char -> Text
reverse (Stream next s0 len0)
    | isEmpty len0 = I.empty
    | otherwise    = I.textP textArr textOff textLen
  where
    len0' = upperBound 4 (larger len0 4)
    (textArr, (textOff, textLen)) = A.run2 $ do
        arr <- A.new len0'
        outer arr len0' s0 (len0' - 1)

    outer arr len = loop
      where
        loop !s !i = case next s of
            Done -> return (arr, (i + 1, len - i - 1))
            Skip s' -> loop s' i
            Yield x s'
                | i < tb -> {-# SCC "reverse/resize" #-} do
                    let newLen = len `shiftL` 1
                    arr' <- A.new newLen
                    A.copyM arr' len arr 0 len
                    outer arr' newLen s (len + i)
                | otherwise -> do
                    d <- unsafeWrite arr (i - tb) x
                    loop s' (i - d)
              where
                tb = U8.charTailBytes x
{-# INLINE [0] reverse #-}

-- | /O(n)/ Perform the equivalent of 'scanr' over a list, only with
-- the input and result reversed.
reverseScanr :: (Char -> Char -> Char) -> Char -> Stream Char -> Stream Char
reverseScanr f z0 (Stream next0 s0 len) = Stream next (S1 :*: z0 :*: s0) (len+1) -- HINT maybe too low
  where
    {-# INLINE next #-}
    next (S1 :*: z :*: s) = Yield z (S2 :*: z :*: s)
    next (S2 :*: z :*: s) = case next0 s of
                              Yield x s' -> let !x' = f x z
                                            in Yield x' (S2 :*: x' :*: s')
                              Skip s'    -> Skip (S2 :*: z :*: s')
                              Done       -> Done
{-# INLINE reverseScanr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a stream from a seed
-- value. However, the length of the result is limited by the
-- first argument to 'unfoldrN'. This function is more efficient than
-- 'unfoldr' when the length of the result is known.
unfoldrN :: Int -> (a -> Maybe (Char,a)) -> a -> Stream Char
unfoldrN n = S.unfoldrNI n
{-# INLINE [0] unfoldrN #-}

-------------------------------------------------------------------------------
-- ** Indexing streams

-- | /O(n)/ stream index (subscript) operator, starting from 0.
index :: Stream Char -> Int -> Char
index = S.indexI
{-# INLINE [0] index #-}

-- | The 'findIndex' function takes a predicate and a stream and
-- returns the index of the first element in the stream
-- satisfying the predicate.
findIndex :: (Char -> Bool) -> Stream Char -> Maybe Int
findIndex = S.findIndexI
{-# INLINE [0] findIndex #-}

-- | /O(n)/ The 'count' function returns the number of times the query
-- element appears in the given stream.
countChar :: Char -> Stream Char -> Int
countChar = S.countCharI
{-# INLINE [0] countChar #-}

-- | /O(n)/ Like a combination of 'map' and 'foldl''. Applies a
-- function to each element of a 'Text', passing an accumulating
-- parameter from left to right, and returns a final 'Text'.
mapAccumL :: (a -> Char -> (a,Char)) -> a -> Stream Char -> (a, Text)
mapAccumL f z0 (Stream next0 s0 len0) = (nz, I.textP na 0 nl)
  where
    (na,(nz,nl)) = A.run2 (A.new mlen >>= \arr -> outer arr mlen z0 s0 0)
      where
        mlen = upperBound 4 len0

    outer arr len = loop
      where
        loop !z !s !i = case next0 s of
            Done          -> return (arr, (z,i))
            Skip s'       -> loop z s' i
            Yield x s'
                | j >= len -> {-# SCC "mapAccumL/resize" #-} do
                    let len' = (len + 1) `shiftL` 1
                    arr' <- A.new len'
                    A.copyM arr' 0 arr 0 len
                    -- We also write the new character here, since we already
                    -- had to calculate it in order to determine j.
                    d <- unsafeWrite arr' i c
                    outer arr' len' z' s' (i + d)
                | otherwise -> do
                    d <- unsafeWrite arr i c
                    loop z' s' (i + d)
              where
                (z', c) = f z x
                j = i + U8.charTailBytes c
{-# INLINE [0] mapAccumL #-}
