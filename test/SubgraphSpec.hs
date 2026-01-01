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
  specSetup $
    describe "Backlinks" $ do
      let baseBacklinkOptions =
            BacklinkOptions
              { blTargets = [],
                blMaxDepth = backlinksDefaultMaxDepth,
                blMinDepth = backlinksDefaultMinDepth
              }

      it "With the default min depth, backlinks does not include the target file" $ \config -> do
        let command = Backlinks $ baseBacklinkOptions {blTargets = [FileTarget $ config.testFiles._linkChain4_md]}
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain3_md]
        mdGraph args >>= outputDoesNotContain [Constants.linkChain4_md]

      it "With a min depth of 0, backlinks _does_ include the target file" $ \config -> do
        let command = Backlinks $ baseBacklinkOptions {blTargets = [FileTarget $ config.testFiles._linkChain4_md], blMinDepth = 0}
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain3_md, Constants.linkChain4_md]

      it "With a min depth of 2, backlinks only includes files 2 links away" $ \config -> do
        let command =
              Backlinks $ BacklinkOptions {blTargets = [FileTarget $ config.testFiles._linkChain4_md], blMinDepth = 2, blMaxDepth = -1}
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputDoesNotContain [Constants.linkChain3_md, Constants.linkChain4_md]
        mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md]

  specSetup $
    describe "Subgraph" $ do
      it "Return the full subgraph of a file" $ \config -> do
        let command =
              Subgraph $ baseSgOptions {sgTargets = [FileTarget $ config.testFiles._linkChain1_md]}
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md, Constants.linkChain3_md, Constants.linkChain4_md]

      it "Max depth is respected" $ \config -> do
        let command =
              Subgraph $ baseSgOptions {sgTargets = [FileTarget $ config.testFiles._linkChain1_md], sgMaxDepth = 3}
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md, Constants.linkChain3_md]
        mdGraph args >>= outputDoesNotContain [Constants.linkChain4_md]

      it "Min depth 2 is respected" $ \config -> do
        let command =
              Subgraph $ baseSgOptions {sgTargets = [FileTarget $ config.testFiles._linkChain1_md], sgMinDepth = 2}
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain3_md, Constants.linkChain4_md]
        mdGraph args >>= outputDoesNotContain [Constants.linkChain1_md, Constants.linkChain2_md]

      it "Min depth 1 is respected" $ \config -> do
        let command =
              Subgraph $ baseSgOptions {sgTargets = [FileTarget $ config.testFiles._linkChain1_md], sgMinDepth = 1}
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain2_md, Constants.linkChain3_md, Constants.linkChain4_md]
        mdGraph args >>= outputDoesNotContain [Constants.linkChain1_md]

      it "Min depth 0 is respected" $ \config -> do
        let command =
              Subgraph $ baseSgOptions {sgTargets = [FileTarget $ config.testFiles._linkChain1_md], sgMinDepth = 0}
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md, Constants.linkChain3_md, Constants.linkChain4_md]

      it "Nonexistent (broken) links are included if requested" $ \config -> do
        let hasNonexistentLink = "has-nonexistent-link.md"
        config.createDoc hasNonexistentLink "[This link doesn't exist](./link-to-nonexistent-file.md)"
        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ hasNonexistentLink],
                    sgInclNonex = True
                  }
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains ["link-to-nonexistent-file.md"]

      it "Nonexistent (broken) links are _not_ included if not requested" $ \config -> do
        let hasNonexistentLink = "has-nonexistent-link.md"
        config.createDoc hasNonexistentLink "[This link doesn't exist](./link-to-nonexistent-file.md)"
        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ hasNonexistentLink]
                  }
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputDoesNotContain ["link-to-nonexistent-file.md"]

      it "Static files (ie files not recognized as documents) are included if requested" $ \config -> do
        let staticpng = "static.png"
        config.createDoc staticpng "This in a image or some other non-document."
        let hasStaticLink = "has-static-link.md"
        config.createDoc hasStaticLink $ mkLink "Link to static" staticpng
        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ hasStaticLink],
                    sgInclStatic = True
                  }
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [staticpng]

      it "Static files (ie files not recognized as documents) are _not_ included if not requested" $ \config -> do
        let staticpng = "static.png"
        config.createDoc staticpng "This in a image or some other non-document."
        let hasStaticLink = "has-static-link.md"
        config.createDoc hasStaticLink $ mkLink "Link to static" staticpng
        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ hasStaticLink],
                    sgInclStatic = False
                  }
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputDoesNotContain [staticpng]

  specSetup $
    describe "Path handling" $ do
      it "Absolute paths are accepted and relativized to the library" $ \config -> do
        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ config.testFiles._linkChain1_md],
                    sgMaxDepth = 1
                  }
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain1_md]

      it "Paths relative to the current directory are accepted and relativized to the library" $ \config -> do
        let libraryName = takeFileName config.specLibraryDir
        withCurrentDirectory "../" $ do
          let command =
                Subgraph $
                  baseSgOptions
                    { sgTargets = [FileTarget $ "./" </> libraryName </> Constants.linkChain1_md]
                    }
          let args = config.defaultArgs {argCommand = command}
          mdGraph args >>= outputContains [Constants.linkChain1_md]

      it "Links with relative directory traversals are resolved and simplified" $ \config -> do
        let usesDirectoryTraversal = "subdir/uses-directory-traversal.md"
        config.createDoc usesDirectoryTraversal $ "[going up a directory](../parent.md)"

        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ usesDirectoryTraversal]
                  }
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.parent_md]

      it "Convoluted directory traversals are resolved and simplified" $ \config -> do
        let usesConvolutedDirectoryTraversal = "subdir/uses-convoluted-directory-traversal.md"
        config.createDoc usesConvolutedDirectoryTraversal $ "[This makes no sense, but go off king](../subdir/../parent.md)"

        let command =
              Subgraph $
                baseSgOptions
                  { sgTargets = [FileTarget $ usesConvolutedDirectoryTraversal]
                  }
        let args = config.defaultArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.parent_md]

      it "Directory and extensions may be omitted in link paths" $
        \config -> do
          let linksDontHaveExtensions = "links-dont-have-extensions.md"
          config.createDoc linksDontHaveExtensions "[This link doesn't have an extension](parent)"
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
          let args = config.defaultArgs {argCommand = command}
          mdGraph args >>= outputContains [Constants.parent_md]

      it "Links may use angle brackets and hint text" $
        \config -> do
          let usesAngleBrackets = "uses-angle-brackets.md"
          config.createDoc usesAngleBrackets "[This link uses angle brackets](<./parent.md> \"this is hint text\")"
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
          let args = config.defaultArgs {argCommand = command}
          mdGraph args >>= outputContains [Constants.parent_md]
