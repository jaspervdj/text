{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
-- |
-- Module      : Data.Text.Unsafe
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
--
-- A module containing unsafe 'Text' operations, for very very careful
-- use in heavily tested code.
module Data.Text.Unsafe
    (
      inlineInterleaveST
    , inlinePerformIO
    , Iter(..)
    , iter
    , iter_
    , reverseIter
    , unsafeHead
    , unsafeTail
    , lengthWord8
    , takeWord8
    , dropWord8
    ) where
     
#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.Text.Encoding.Utf8 (chrUtf8)
import Data.Text.Internal (Text(..))
import GHC.ST (ST(..))
import qualified Data.Text.Array as A
#if defined(__GLASGOW_HASKELL__)
# if __GLASGOW_HASKELL__ >= 611
import GHC.IO (IO(IO))
# else
import GHC.IOBase (IO(IO))
# endif
import GHC.Base (realWorld#)
#endif

-- | /O(1)/ A variant of 'head' for non-empty 'Text'. 'unsafeHead'
-- omits the check for the empty case, so there is an obligation on
-- the programmer to provide a proof that the 'Text' is non-empty.
unsafeHead :: Text -> Char
unsafeHead (Text arr off _len) = chrUtf8 (\c _ -> c) n1 n2 n3 n4
  where
    n1 = A.unsafeIndex arr off
    n2 = A.unsafeIndex arr (off + 1)
    n3 = A.unsafeIndex arr (off + 2)
    n4 = A.unsafeIndex arr (off + 3)
{-# INLINE unsafeHead #-}

-- | /O(1)/ A variant of 'tail' for non-empty 'Text'. 'unsafeHead'
-- omits the check for the empty case, so there is an obligation on
-- the programmer to provide a proof that the 'Text' is non-empty.
unsafeTail :: Text -> Text
unsafeTail t@(Text arr off len) =
#if defined(ASSERTS)
    assert (d <= len) $
#endif
    Text arr (off+d) (len-d)
  where d = iter_ t 0
{-# INLINE unsafeTail #-}

data Iter = Iter {-# UNPACK #-} !Char {-# UNPACK #-} !Int

-- | /O(1)/ Iterate (unsafely) one step forwards through a UTF-16
-- array, returning the current character and the delta to add to give
-- the next offset to iterate at.
iter :: Text -> Int -> Iter
iter (Text arr off _len) i = chrUtf8 (\c d -> Iter c d) n1 n2 n3 n4
  where
    j = off + i
    n1 = A.unsafeIndex arr j
    n2 = A.unsafeIndex arr (j + 1)
    n3 = A.unsafeIndex arr (j + 2)
    n4 = A.unsafeIndex arr (j + 3)
{-# INLINE iter #-}

-- | /O(1)/ Iterate one step through a UTF-16 array, returning the
-- delta to add to give the next offset to iterate at.
iter_ :: Text -> Int -> Int
iter_ (Text arr off _len) i
    | m < 0xC0  = 1
    | m < 0xE0  = 2
    | m < 0xF0  = 3
    | otherwise = 4
  where
    m = A.unsafeIndex arr (off+i)
{-# INLINE iter_ #-}

-- | /O(1)/ Iterate one step backwards through a UTF-16 array,
-- returning the current character and the delta to add (i.e. a
-- negative number) to give the next offset to iterate at.
reverseIter :: Text -> Int -> (Char,Int)
reverseIter (Text arr off _len) i = chrUtf8 (,) n1 n2 n3 n4
  where
    n1 = A.unsafeIndex arr j
    n2 = A.unsafeIndex arr (j + 1)
    n3 = A.unsafeIndex arr (j + 2)
    n4 = A.unsafeIndex arr (j + 3)
    j = off + i
{-# INLINE reverseIter #-}

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block. On Hugs this is just @unsafePerformIO@.
--
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif

-- | Allow an 'ST' computation to be deferred lazily. When passed an
-- action of type 'ST' @s@ @a@, the action will only be performed when
-- the value of @a@ is demanded.
--
-- This function is identical to the normal unsafeInterleaveST, but is
-- inlined and hence faster.
--
-- /Note/: This operation is highly unsafe, as it can introduce
-- externally visible non-determinism into an 'ST' action.
inlineInterleaveST :: ST s a -> ST s a
inlineInterleaveST (ST m) = ST $ \ s ->
    let r = case m s of (# _, res #) -> res in (# s, r #)
{-# INLINE inlineInterleaveST #-}

-- | /O(1)/ Return the length of a 'Text' in units of 'Word8'.  This
-- is useful for sizing a target array appropriately before using
-- 'unsafeCopyToPtr'.
lengthWord8 :: Text -> Int
lengthWord8 (Text _arr _off len) = len
{-# INLINE lengthWord8 #-}

-- | /O(1)/ Unchecked take of 'k' 'Word16's from the front of a 'Text'.
takeWord8 :: Int -> Text -> Text
takeWord8 k (Text arr off _len) = Text arr off k
{-# INLINE takeWord8 #-}

-- | /O(1)/ Unchecked drop of 'k' 'Word16's from the front of a 'Text'.
dropWord8 :: Int -> Text -> Text
dropWord8 k (Text arr off len) = Text arr (off+k) (len-k)
{-# INLINE dropWord8 #-}
