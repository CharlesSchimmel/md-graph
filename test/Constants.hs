module Constants where

import System.FilePath

linkChain1_md = "link chain 1.md"

linkChain2_md = "link chain 2.md"

linkChain3_md = "link chain 3.md"

linkChain4_md = "link chain 4.md"

parent_md = "parent.md"

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
