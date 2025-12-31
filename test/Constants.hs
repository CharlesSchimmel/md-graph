module Constants where

import System.FilePath

linkChain1_md = "link chain 1.md"

linkChain2_md = "link chain 2.md"

linkChain3_md = "link chain 3.md"

linkChain4_md = "link chain 4.md"

parent_md = "parent.md"

orphan_md = "orphan.md"

usesDirectoryTraversal_md = "subdir/uses directory traversal.md"

usesConvolutedDirectoryTraversal_md = "subdir/uses convoluted directory traversal.md"

unreachable_md = "unreachable.md"

linksDontHaveExtensions_md = "links-dont-have-extensions.md"

angleBrackets_md = "angle-brackets.md"

hasNonExistentLink_md = "has-nonexistent-link.md"

static_txt = "static.txt"

hasStaticFileLink_md = "has-static-file-link.md"

data TestFiles = TestFiles
  { _linkChain1_md :: FilePath,
    _linkChain2_md :: FilePath,
    _linkChain3_md :: FilePath,
    _linkChain4_md :: FilePath,
    _parent_md :: FilePath,
    _orphan_md :: FilePath,
    _usesDirectoryTraversal_md :: FilePath,
    _usesConvolutedDirectoryTraversal_md :: FilePath,
    _unreachable_md :: FilePath,
    _linksDontHaveExtensions_md :: FilePath,
    _angleBrackets_md :: FilePath,
    _hasNonExistentLink_md :: FilePath,
    _static_txt :: FilePath,
    _hasStaticFileLink_md :: FilePath
  }
  deriving (Show)

testFiles :: FilePath -> TestFiles
testFiles baseDir =
  TestFiles
    { _linkChain1_md = baseDir </> "link chain 1.md",
      _linkChain2_md = baseDir </> "link chain 2.md",
      _linkChain3_md = baseDir </> "link chain 3.md",
      _linkChain4_md = baseDir </> "link chain 4.md",
      _parent_md = baseDir </> "parent.md",
      _orphan_md = baseDir </> "orphan.md",
      _usesDirectoryTraversal_md = baseDir </> "subdir/uses directory traversal.md",
      _usesConvolutedDirectoryTraversal_md = baseDir </> "subdir/uses convoluted directory traversal.md",
      _unreachable_md = baseDir </> "unreachable.md",
      _linksDontHaveExtensions_md = baseDir </> "links-dont-have-extensions.md",
      _angleBrackets_md = baseDir </> "angle-brackets.md",
      _hasNonExistentLink_md = baseDir </> "has-nonexistent-link.md",
      _static_txt = baseDir </> "static.txt",
      _hasStaticFileLink_md = baseDir </> "has-static-file-link.md"
    }
