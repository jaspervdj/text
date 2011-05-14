-- | This benchmark converts a number of UTF-8 encoded files to uppercase.
-- It takes the list of files on stdin, one filename on each line. It then uses
-- the criterion library to run a the benchmark on each file & get reliable
-- results.
--
import Control.Exception (evaluate)
import Control.Applicative ((<$>))
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

import Criterion.Main

main :: IO ()
main = do
    files <- lines <$> getContents
    defaultMain =<< mapM bench' files
  where
    -- Read & evaluate the entire file, then convert it to upper case in a pure
    -- benchmark.
    bench' file = do
        t <- T.decodeUtf8 `fmap` B.readFile file
        _ <- evaluate t
        return $ bench ("CaseMap " ++ file) $ nf T.toUpper t
