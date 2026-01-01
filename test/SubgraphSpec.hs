{-# LANGUAGE OverloadedRecordDot #-}

{-# HLINT ignore "Eta reduce" #-}

module SubgraphSpec (spec) where

import qualified Constants
import MdGraph
import MdGraph.App.Arguments
import MdGraph.App.Command
import qualified MdGraph.TagDirection as TagDirection
import Spec.Base
import System.Directory (withCurrentDirectory)
import System.FilePath
import Test.Hspec

main :: IO ()
main = do
  hspec $ spec

spec :: Spec
spec = do
  setupSpecEnv $
    describe "Backlinks" $ do
      let baseBacklinkOptions =
            BacklinkOptions
              { blTargets = [],
                blMaxDepth = backlinksDefaultMaxDepth,
                blMinDepth = backlinksDefaultMinDepth
              }

      it "With the default min depth, backlinks does not include the target file" $ \env -> do
        let command = Backlinks $ baseBacklinkOptions {blTargets = [FileTarget $ env.testFiles._linkChain4_md]}
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain3_md]
        mdGraph args >>= outputDoesNotContain [Constants.linkChain4_md]

      it "With a min depth of 0, backlinks _does_ include the target file" $ \env -> do
        let command = Backlinks $ baseBacklinkOptions {blTargets = [FileTarget $ env.testFiles._linkChain4_md], blMinDepth = 0}
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain3_md, Constants.linkChain4_md]

      it "With a min depth of 2, backlinks only includes files 2 links away" $ \env -> do
        let command =
              Backlinks $ BacklinkOptions {blTargets = [FileTarget $ env.testFiles._linkChain4_md], blMinDepth = 2, blMaxDepth = -1}
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputDoesNotContain [Constants.linkChain3_md, Constants.linkChain4_md]
        mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md]

  setupSpecEnv $
    describe "Subgraph" $ do
      it "Return the full subgraph of a file" $ \env -> do
        let command =
              Subgraph $ baseSgOptions {sgTargets = [FileTarget $ env.testFiles._linkChain1_md]}
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md, Constants.linkChain3_md, Constants.linkChain4_md]

      it "Max depth is respected" $ \env -> do
        let command =
              Subgraph $ baseSgOptions {sgTargets = [FileTarget $ env.testFiles._linkChain1_md], sgMaxDepth = 3}
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md, Constants.linkChain3_md]
        mdGraph args >>= outputDoesNotContain [Constants.linkChain4_md]

      it "Min depth 2 is respected" $ \env -> do
        let command =
              Subgraph $ baseSgOptions {sgTargets = [FileTarget $ env.testFiles._linkChain1_md], sgMinDepth = 2}
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain3_md, Constants.linkChain4_md]
        mdGraph args >>= outputDoesNotContain [Constants.linkChain1_md, Constants.linkChain2_md]

      it "Min depth 1 is respected" $ \env -> do
        let command =
              Subgraph $ baseSgOptions {sgTargets = [FileTarget $ env.testFiles._linkChain1_md], sgMinDepth = 1}
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain2_md, Constants.linkChain3_md, Constants.linkChain4_md]
        mdGraph args >>= outputDoesNotContain [Constants.linkChain1_md]

      it "Min depth 0 is respected" $ \env -> do
        let command =
              Subgraph $ baseSgOptions {sgTargets = [FileTarget $ env.testFiles._linkChain1_md], sgMinDepth = 0}
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md, Constants.linkChain3_md, Constants.linkChain4_md]

      it "Nonexistent (broken) links are included if requested" $ \env -> do
        let hasNonexistentLink = "has-nonexistent-link.md"
        env.createDoc hasNonexistentLink "[This link doesn't exist](./link-to-nonexistent-file.md)"
        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ hasNonexistentLink],
                    sgInclNonex = True
                  }
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains ["link-to-nonexistent-file.md"]

      it "Nonexistent (broken) links are _not_ included if not requested" $ \env -> do
        let hasNonexistentLink = "has-nonexistent-link.md"
        env.createDoc hasNonexistentLink "[This link doesn't exist](./link-to-nonexistent-file.md)"
        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ hasNonexistentLink]
                  }
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputDoesNotContain ["link-to-nonexistent-file.md"]

      it "Static files (ie files not recognized as documents) are included if requested" $ \env -> do
        let staticpng = "static.png"
        env.createDoc staticpng "This in a image or some other non-document."
        let hasStaticLink = "has-static-link.md"
        env.createDoc hasStaticLink $ mkLink "Link to static" staticpng
        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ hasStaticLink],
                    sgInclStatic = True
                  }
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [staticpng]

      it "Static files (ie files not recognized as documents) are _not_ included if not requested" $ \env -> do
        let staticpng = "static.png"
        env.createDoc staticpng "This in a image or some other non-document."
        let hasStaticLink = "has-static-link.md"
        env.createDoc hasStaticLink $ mkLink "Link to static" staticpng
        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ hasStaticLink],
                    sgInclStatic = False
                  }
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputDoesNotContain [staticpng]

  setupSpecEnv $
    describe "Path handling" $ do
      it "Absolute paths are accepted and relativized to the library" $ \env -> do
        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ env.testFiles._linkChain1_md],
                    sgMaxDepth = 1
                  }
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain1_md]

      it "Paths relative to the current directory are accepted and relativized to the library" $ \env -> do
        let libraryName = takeFileName env.libraryDir
        withCurrentDirectory "../" $ do
          let command =
                Subgraph $
                  baseSgOptions
                    { sgTargets = [FileTarget $ "./" </> libraryName </> Constants.linkChain1_md]
                    }
          let args = env.defaultArgs {argCommand = command}
          mdGraph args >>= outputContains [Constants.linkChain1_md]

      it "Links with relative directory traversals are resolved and simplified" $ \env -> do
        let usesDirectoryTraversal = "subdir/uses-directory-traversal.md"
        env.createDoc usesDirectoryTraversal $ "[going up a directory](../parent.md)"

        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ usesDirectoryTraversal]
                  }
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.parent_md]

      it "Convoluted directory traversals are resolved and simplified" $ \env -> do
        let usesConvolutedDirectoryTraversal = "subdir/uses-convoluted-directory-traversal.md"
        env.createDoc usesConvolutedDirectoryTraversal $ "[This makes no sense, but go off king](../subdir/../parent.md)"

        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ usesConvolutedDirectoryTraversal]
                  }
        let args = env.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.parent_md]

      it "Directory and extensions may be omitted in link paths" $
        \env -> do
          let linksDontHaveExtensions = "links-dont-have-extensions.md"
          env.createDoc linksDontHaveExtensions "[This link doesn't have an extension](parent)"
          let command =
                Subgraph $
                  SubgraphOptions
                    { sgTargets = [FileTarget $ linksDontHaveExtensions],
                      sgInclNonex = False, -- Important for this test to ensure "./parent" is recognized as a link to a document
                      sgInclStatic = False, -- Important for this test
                      sgTagDir = TagDirection.In,
                      sgMaxDepth = -1,
                      sgMinDepth = -1
                    }
          let args = env.defaultArgs {argCommand = command}
          mdGraph args >>= outputContains [Constants.parent_md]

      it "Links may use angle brackets and hint text" $
        \env -> do
          let usesAngleBrackets = "uses-angle-brackets.md"
          env.createDoc usesAngleBrackets "[This link uses angle brackets](<./parent.md> \"this is hint text\")"
          let command =
                Subgraph $
                  SubgraphOptions
                    { sgTargets = [FileTarget $ usesAngleBrackets],
                      sgInclNonex = True,
                      sgInclStatic = True,
                      sgTagDir = TagDirection.In,
                      sgMaxDepth = -1,
                      sgMinDepth = -1
                    }
          let args = env.defaultArgs {argCommand = command}
          mdGraph args >>= outputContains [Constants.parent_md]
