module Constants where

import System.FilePath

data TestFiles = TestFiles
  { linkChain1_md :: FilePath,
    linkChain2_md :: FilePath,
    linkChain3_md :: FilePath,
    linkChain4_md :: FilePath,
    parent_md :: FilePath,
    orphan_md :: FilePath,
    usesDirectoryTraversal_md :: FilePath,
    usesConvolutedDirectoryTraversal_md :: FilePath,
    unreachable_md :: FilePath,
    linksDontHaveExtensions_md :: FilePath,
    angleBrackets_md :: FilePath,
    hasNonExistentLink_md :: FilePath,
    static_txt :: FilePath,
    hasStaticFileLink_md :: FilePath
  }
  deriving (Show)

testFiles :: FilePath -> TestFiles
testFiles baseDir =
  TestFiles
    { linkChain1_md = baseDir </> "link chain 1.md",
      linkChain2_md = baseDir </> "link chain 2.md",
      linkChain3_md = baseDir </> "link chain 3.md",
      linkChain4_md = baseDir </> "link chain 4.md",
      parent_md = baseDir </> "parent.md",
      orphan_md = baseDir </> "orphan.md",
      usesDirectoryTraversal_md = baseDir </> "subdir/uses directory traversal.md",
      usesConvolutedDirectoryTraversal_md = baseDir </> "subdir/uses convoluted directory traversal.md",
      unreachable_md = baseDir </> "unreachable.md",
      linksDontHaveExtensions_md = baseDir </> "links-dont-have-extensions.md",
      angleBrackets_md = baseDir </> "angle-brackets.md",
      hasNonExistentLink_md = baseDir </> "has-nonexistent-link.md",
      static_txt = baseDir </> "static.txt",
      hasStaticFileLink_md = baseDir </> "has-static-file-link.md"
    }
