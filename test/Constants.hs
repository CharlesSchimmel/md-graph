{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Constants where

import MdGraph.File
import System.Directory
import System.FilePath

linkChain1_md = "link chain 1.md"

linkChain2_md = "link chain 2.md"

linkChain3_md = "link chain 3.md"

linkChain4_md = "link chain 4.md"

parent_md = "parent.md"

data TestFile = TestFile
  { absolute :: FilePath,
    pathFromLibrary :: FilePath,
    pathFromCurrent :: IO FilePath
  }

data TestFiles = TestFiles
  { _linkChain1_md :: FilePath,
    _linkChain2_md :: FilePath,
    _linkChain3_md :: FilePath,
    _linkChain4_md :: FilePath,
    _parent_md :: FilePath
  }
  deriving (Show)

testFiles :: FilePath -> TestFiles
testFiles baseDir =
  TestFiles
    { _linkChain1_md = baseDir </> "link chain 1.md",
      _linkChain2_md = baseDir </> "link chain 2.md",
      _linkChain3_md = baseDir </> "link chain 3.md",
      _linkChain4_md = baseDir </> "link chain 4.md",
      _parent_md = baseDir </> "parent.md"
    }

getPathFromCurrent :: FilePath -> IO FilePath
getPathFromCurrent path = do
  current <- getCurrentDirectory
  return $ makeRelativeTraversal current path

mkTestFile libDir testFileName =
  let absPath = libDir </> testFileName
   in TestFile
        { pathFromLibrary = testFileName,
          absolute = absPath,
          pathFromCurrent = getPathFromCurrent absPath
        }

data TestFiles' = TestFiles'
  { linkChain1 :: TestFile,
    linkChain2 :: TestFile,
    linkChain3 :: TestFile,
    linkChain4 :: TestFile,
    parent :: TestFile
  }

testFiles' :: FilePath -> TestFiles'
testFiles' baseDir =
  TestFiles'
    { linkChain1 = mkTestFile baseDir "link-chain-1.md",
      linkChain2 = mkTestFile baseDir "link-chain-2.md",
      linkChain3 = mkTestFile baseDir "link-chain-3.md",
      linkChain4 = mkTestFile baseDir "link-chain-4.md",
      parent = mkTestFile baseDir "parent.md"
    }
