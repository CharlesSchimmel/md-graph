{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Constants
import Data.Either
import Data.Function ((&))
import qualified Data.Text.IO as Text
import Database.Esqueleto.Experimental
import Database.Persist.Sqlite (Entity (entityVal), runSqlite)
import qualified FilesSpec
import MdGraph (mdGraph)
import MdGraph.App.Arguments
import MdGraph.App.Command as Command
import qualified MdGraph.App.LogLevel as LogLevel
import MdGraph.App.RunCommand (runCommand)
import MdGraph.Config (Config (Config, libraryPath))
import MdGraph.File (Files (..), isAncestorOf, normaliseEvil, unrelativize)
import MdGraph.File.Types (AbsolutePath (..), File (..))
import MdGraph.Node (Link (..))
import MdGraph.Persist.Class (Queries (getForwardLinks))
import MdGraph.Persist.Query (forwardLinks, getAllDocuments, orphansM, unreachableM)
import MdGraph.Persist.Schema (Document (documentPath), EntityField (..))
import qualified MdGraph.TagDirection as TagDirection
import Spec.Base
import qualified SubgraphSpec
import System.Directory (getCurrentDirectory, getTemporaryDirectory, removeFile)
import System.FilePath
import Test.Hspec

main :: IO ()
main = do
  libraryDir <- getLibraryDir
  hspec $ do
    FilesSpec.spec
    SubgraphSpec.spec libraryDir
    populateSpec
    -- let Constants.TestFiles {..} = testFiles libraryDir

    let baseSgOptions =
          SubgraphOptions
            { sgInclNonex = False,
              sgInclStatic = False,
              sgTagDir = TagDirection.In,
              sgMaxDepth = subgraphDefaultMaxDepth,
              sgTargets = [],
              sgMinDepth = subgraphDefaultMinDepth
            }
    specSetup $
      describe "Path handling" $ do
        it "Absolute paths are accepted and relativized to the library" $
          \SpecConfig {..} -> do
            let command =
                  Subgraph $
                    baseSgOptions
                      { sgTargets = [FileTarget $ testFiles._linkChain1_md],
                        sgMaxDepth = 1
                      }
            let args = defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [takeFileName testFiles._linkChain1_md]

        it "Paths relative to the current directory are accepted and relativized to the library" $
          \SpecConfig {..} -> do
            let command =
                  Subgraph $
                    baseSgOptions
                      { sgTargets = [FileTarget $ testFiles._linkChain1_md]
                      }
            let args = defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [takeFileName testFiles._linkChain1_md]

        it "Relative directory traversals are resolved and simplified" $
          \SpecConfig {..} -> do
            let command =
                  Subgraph $
                    baseSgOptions
                      { sgTargets = [FileTarget $ testFiles._usesDirectoryTraversal_md]
                      }
            let args = defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [takeFileName testFiles._parent_md]

        it "Convoluted directory traversals are resolved and simplified" $
          \SpecConfig {..} -> do
            let command =
                  Subgraph $
                    baseSgOptions
                      { sgTargets = [FileTarget $ testFiles._usesConvolutedDirectoryTraversal_md]
                      }
            let args = defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [takeFileName testFiles._parent_md]

    specSetup $
      describe "Orphans" $ do
        it "Files with no links to or from them are identified" $
          \SpecConfig {..} -> do
            let args = defaultArgs {argCommand = Command.Orphans}
            mdGraph args >>= outputContains [Constants.orphan_md]

        it "Orphan files are not identified as unreachable" $
          \SpecConfig {..} -> do
            let args = defaultArgs {argCommand = Command.Unreachable}
            mdGraph args >>= outputDoesNotContain [Constants.orphan_md]

    specSetup $
      describe "Unreachable" $ do
        it "Files that have links but have no links to them are identified" $
          \config -> do
            let args = config.defaultArgs {argCommand = Command.Unreachable}
            mdGraph args >>= outputContains [Constants.unreachable_md]

    specSetup $
      describe "Nonexistent" $ do
        it "Nonexistent returns links that do not resolve to a file" $
          \config -> do
            let args = config.defaultArgs {argCommand = Command.Nonexes}
            mdGraph args >>= outputContains ["link-to-nonexistent-file.md"]

    specSetup $
      describe "Parsing" $ do
        it "File extensions in links may be omitted and the default is used instead" $
          \config -> do
            let command =
                  Subgraph $
                    SubgraphOptions
                      { sgTargets = [FileTarget $ config.testFiles._linksDontHaveExtensions_md],
                        sgInclNonex = True,
                        sgInclStatic = True,
                        sgTagDir = TagDirection.In,
                        sgMaxDepth = -1,
                        sgMinDepth = -1
                      }
            let args = config.defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [Constants.parent_md]

        it "Links may use angle brackets and hint text" $
          \config -> do
            let command =
                  Subgraph $
                    SubgraphOptions
                      { sgTargets = [FileTarget $ config.testFiles._angleBrackets_md],
                        sgInclNonex = True,
                        sgInclStatic = True,
                        sgTagDir = TagDirection.In,
                        sgMaxDepth = -1,
                        sgMinDepth = -1
                      }
            let args = config.defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [Constants.parent_md]

populateSpec :: Spec
populateSpec = do
  specSetup $
    describe "Scan Options" $ do
      it "User can specify specific files to scan" $
        \config -> do
          -- Populate only linkChain2_md
          let scanOpt = ScanSome [config.testFiles._linkChain2_md]
          let args' = config.defaultArgs {argCommand = Populate, argScan = scanOpt}
          mdGraph args'

          -- It should be the only document in the database, even though it has forward and backward links and there are more in the library
          rawQueryResults <- runSqlite config.dbPath $ getAllDocuments
          let dbDocuments = fmap documentPath $ entityVal <$> rawQueryResults
          dbDocuments `shouldContain` [Constants.linkChain2_md]
          dbDocuments `shouldNotContain` [Constants.linkChain1_md]

          -- Populate linkChain1
          let scanOpt = ScanSome [config.testFiles._linkChain1_md]
          let args' = config.defaultArgs {argCommand = Populate, argScan = scanOpt}
          _ <- mdGraph args'

          -- Get the forwardLinks of linkChain1_md, it should contain linkChain2_md, but no others.
          rawQueryResults <- runSqlite config.dbPath $ forwardLinks Constants.linkChain1_md
          let queryResultPaths = fmap documentPath $ entityVal <$> rights rawQueryResults
          queryResultPaths `shouldContain` [Constants.linkChain2_md]
          queryResultPaths `shouldNotContain` [Constants.linkChain3_md]
