module Util
  ( -- * Data Types
    ProgramFile (..),
    Dir,

    -- * Fun
    getCPPFilePath,
    getCPPFiles,
    readFile',
  )
where

--------------------------------------------------

import Control.DeepSeq
import Control.Exception (evaluate)
import Data.List (isSuffixOf, sort)
import Data.Traversable (for)
import System.Directory
import System.FilePath
import System.IO (IOMode (..), hClose, hGetContents, openFile)

--------------------------------------------------

type Dir = FilePath

data ProgramFile = ProgramFile
  { name :: String,
    source :: String
  }

instance Eq ProgramFile where
  (ProgramFile name1 _) == (ProgramFile name2 _) = name1 == name2

instance Ord ProgramFile where
  (ProgramFile name1 _) `compare` (ProgramFile name2 _) = name1 `compare` name2

getCPPFilePath :: Dir -> IO [FilePath]
getCPPFilePath dir = do
  fps <- filter (isSuffixOf ".cc") <$> listDirectory dir
  return ((dir </>) <$> fps)

getCPPFiles :: Dir -> IO [ProgramFile]
getCPPFiles dir = do
  fps <- filter (isSuffixOf ".cc") <$> listDirectory dir
  rs <- for fps $ \fp -> do
    let name = takeWhile (/= '.') fp
    src <- readFile' (dir </> fp)
    src `deepseq` return (ProgramFile name src)
  return (sort rs)

readFile' :: FilePath -> IO String
readFile' fn = do
  h <- openFile fn ReadMode
  s <- hGetContents h
  evaluate (rnf s)
  hClose h
  return s
