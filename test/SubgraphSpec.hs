module SubgraphSpec (spec) where

{-# HLINT ignore "Eta reduce" #-}
import qualified Constants
import MdGraph
import MdGraph.App.Arguments
import MdGraph.App.Command
import qualified MdGraph.TagDirection as TagDirection
import Spec.Base
import System.FilePath
import Test.Hspec

main :: IO ()
main = do
  libraryDir <- getLibraryDir
  hspec $ spec libraryDir

spec :: FilePath -> Spec
spec libraryDir = do
  let baseSgOptions =
        SubgraphOptions
          { sgInclNonex = False,
            sgInclStatic = False,
            sgTagDir = TagDirection.In,
            sgMaxDepth = subgraphDefaultMaxDepth,
            sgTargets = [],
            sgMinDepth = subgraphDefaultMinDepth
          }
  describe "Backlinks" $ do
    let baseBacklinkOptions =
          BacklinkOptions
            { blTargets = [],
              blMaxDepth = backlinksDefaultMaxDepth,
              blMinDepth = backlinksDefaultMinDepth
            }

    it "Correct backlinks are returned with default min depth" $ do
      let command = Backlinks $ baseBacklinkOptions {blTargets = [FileTarget $ libraryDir </> Constants.linkChain4_md]}
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains [Constants.linkChain3_md]
      mdGraph args >>= outputDoesNotContain [Constants.linkChain4_md]

    it "Correct backlinks are returned with minDepth 0" $ do
      let command = Backlinks $ baseBacklinkOptions {blTargets = [FileTarget $ libraryDir </> Constants.linkChain4_md], blMinDepth = 0}
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains [Constants.linkChain3_md, Constants.linkChain4_md]

    it "Min depth 2 is respected" $ do
      let command =
            Backlinks $ BacklinkOptions {blTargets = [FileTarget $ libraryDir </> Constants.linkChain4_md], blMinDepth = 2, blMaxDepth = -1}
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputDoesNotContain [Constants.linkChain3_md, Constants.linkChain4_md]
      mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md]

  describe "Subgraph" $ do
    it "Return the full subgraph of a file" $ do
      let command =
            Subgraph $ baseSgOptions {sgTargets = [FileTarget $ libraryDir </> Constants.linkChain1_md]}
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md, Constants.linkChain3_md, Constants.linkChain4_md]

    it "Max depth is respected" $ do
      let command =
            Subgraph $ baseSgOptions {sgTargets = [FileTarget $ libraryDir </> Constants.linkChain1_md], sgMaxDepth = 3}
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md, Constants.linkChain3_md]
      mdGraph args >>= outputDoesNotContain [Constants.linkChain4_md]

    it "Min depth 2 is respected" $ do
      let command =
            Subgraph $ baseSgOptions {sgTargets = [FileTarget $ libraryDir </> Constants.linkChain1_md], sgMinDepth = 2}
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains [Constants.linkChain3_md, Constants.linkChain4_md]
      mdGraph args >>= outputDoesNotContain [Constants.linkChain1_md, Constants.linkChain2_md]

    it "Min depth 1 is respected" $ do
      let command =
            Subgraph $ baseSgOptions {sgTargets = [FileTarget $ libraryDir </> Constants.linkChain1_md], sgMinDepth = 1}
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains [Constants.linkChain2_md, Constants.linkChain3_md, Constants.linkChain4_md]
      mdGraph args >>= outputDoesNotContain [Constants.linkChain1_md]

    it "Min depth 0 is respected" $ do
      let command =
            Subgraph $ baseSgOptions {sgTargets = [FileTarget $ libraryDir </> Constants.linkChain1_md], sgMinDepth = 0}
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md, Constants.linkChain3_md, Constants.linkChain4_md]

    it "Nonexistent (broken) links are included if requested" $ do
      let command =
            Subgraph $
              baseSgOptions
                { sgTargets = [FileTarget $ libraryDir </> Constants.hasNonExistentLink_md],
                  sgInclNonex = True
                }
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains ["link-to-nonexistent-file.md"]

    it "Nonexistent (broken) links are _not_ included if not requested" $ do
      let command =
            Subgraph $
              baseSgOptions
                { sgTargets = [FileTarget $ libraryDir </> Constants.hasNonExistentLink_md]
                }
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputDoesNotContain ["link-to-nonexistent-file.md"]

    it "Static files (ie files not recognized as documents) are included if requested" $ do
      let command =
            Subgraph $
              baseSgOptions
                { sgTargets = [FileTarget $ libraryDir </> Constants.hasStaticFileLink_md],
                  sgInclStatic = True
                }
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains [Constants.static_txt]

    it "Static files (ie files not recognized as documents) are _not_ included if not requested" $ do
      let command =
            Subgraph $
              baseSgOptions
                { sgTargets = [FileTarget $ libraryDir </> Constants.hasStaticFileLink_md]
                }
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputDoesNotContain [Constants.static_txt]

  describe "Path handling" $ do
    it "Absolute paths are accepted and relativized to the library" $ do
      let command =
            Subgraph $
              baseSgOptions
                { sgTargets = [FileTarget $ libraryDir </> Constants.linkChain1_md],
                  sgMaxDepth = 1
                }
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains [Constants.linkChain1_md]

    it "Paths relative to the current directory are accepted and relativized to the library" $ do
      let command =
            Subgraph $
              baseSgOptions
                { sgTargets = [FileTarget $ "./test/library" </> Constants.linkChain1_md]
                }
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains [Constants.linkChain1_md]

    it "Relative directory traversals are resolved and simplified" $ do
      let command =
            Subgraph $
              baseSgOptions
                { sgTargets = [FileTarget $ libraryDir </> Constants.usesDirectoryTraversal_md]
                }
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains [Constants.parent_md]

    it "Convoluted directory traversals are resolved and simplified" $ do
      let command =
            Subgraph $
              baseSgOptions
                { sgTargets = [FileTarget $ libraryDir </> Constants.usesConvolutedDirectoryTraversal_md]
                }
      let args = defaultSpecArgs {argCommand = command}
      mdGraph args >>= outputContains [Constants.parent_md]
