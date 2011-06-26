-- | Some tests for the Array module
--
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Text.Tests.Array
    (
      tests
    ) where

import Data.Word (Word8)

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.Text.Array as A

data ArrayLength = ArrayLength A.Array Int

instance Arbitrary ArrayLength where
    arbitrary = arbitrary >>= \list ->
        return $ ArrayLength (fromList list) (length list)

instance Show ArrayLength where
    show (ArrayLength a l) = "ArrayLength (fromList " ++
        show (A.toList a 0 l) ++ ") " ++ show l

fromList :: [Word8] -> A.Array
fromList list = A.run $ do
    a <- A.new (length list)
    write a 0 list
    return a
  where
    write _ _ []       = return ()
    write a i (x : xs) = A.unsafeWrite a i x >> write a (i + 1) xs

a_fromlist_tolist l = l == (A.toList (fromList l) 0 (length l))

copy :: A.Array -> Int -> A.Array
copy a l = A.run $ do
    a' <- A.new l
    A.copyI a' 0 a 0 l
    return a'

a_copy (ArrayLength a l) = let a' = copy a l in A.equal a 0 a' 0 l

tests :: Test
tests =
  testGroup "Array" [
    testProperty "a_fromlist_tolist" a_fromlist_tolist,
    testProperty "a_copy"            a_copy
  ]
