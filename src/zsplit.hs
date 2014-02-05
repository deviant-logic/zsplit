{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Codec.Compression.GZip
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Int
import           System.Environment

main :: IO ()
main = getArgs >>= zsplit . options

zsplit :: Options -> IO ()
zsplit Options {..} =
  do input <- maybe BL.getContents BL.readFile input_file
     let dc'd   = decompress input
         splits = chunks (line_offsets line_count dc'd) dc'd
	 files  = filenames prefix suffix_length
     zipWithM_ BL.writeFile files (map compress splits)

chunks :: [Int64] -> ByteString -> [ByteString]
chunks _      "" = []
chunks []     s  = [s]
chunks (o:os) s  = BL.take o s : chunks (fmap (subtract o) os) (BL.drop o s)

line_offsets :: Int -> ByteString -> [Int64]
line_offsets n s = every_nth n (fmap succ $ BL.elemIndices '\n' s)

every_nth :: Int -> [a] -> [a]
every_nth n xs | n < 2 = xs
               | otherwise = case drop (pred n) xs of
			       []         -> []
			       (nth:rest) -> nth : every_nth n rest

filenames :: FilePath -> Int -> [FilePath]
filenames prefix n = [prefix ++ suffix | suffix <- suffixes n]
  where suffixes 0 = [".gz"]
	suffixes n = [x:xs | x  <- ['a'..'z'], xs <- suffixes (pred n)]

data Options = Options {
  input_file    :: Maybe FilePath,
  prefix        :: FilePath,
  suffix_length :: Int,
  line_count    :: Int
} deriving (Eq, Ord, Show)

default_options :: Options
default_options = Options Nothing "x" 2 1000

options :: [String] -> Options
options []          = default_options
options ("-a":x:xs) = (options xs)     { suffix_length = read x }
options ("-l":x:xs) = (options xs)     { line_count    = read x }
options ["-"]       = options []
options [file]      = (options [])     { input_file = Just file }
options [file, p]   = (options [file]) { prefix = p }
options xs          = error $ "couldn't process options: " ++ show xs
