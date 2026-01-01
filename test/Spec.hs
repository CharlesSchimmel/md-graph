{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Constants
import Data.Either
import Data.Function ((&))
import qualified Data.Text as Text
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
  hspec $ do
    FilesSpec.spec
    SubgraphSpec.spec
    populateSpec
    -- let Constants.TestFiles {..} = testFiles libraryDir

    specSetup $
      describe "Path handling" $ do
        it "Absolute paths are accepted and relativized to the library" $
          \SpecEnv {..} -> do
            let command =
                  Subgraph $
                    baseSgOptions
                      { sgTargets = [FileTarget $ testFiles._linkChain1_md],
                        sgMaxDepth = 1
                      }
            let args = defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [takeFileName testFiles._linkChain1_md]

        it "Paths relative to the current directory are accepted and relativized to the library" $
          \SpecEnv {..} -> do
            let command =
                  Subgraph $
                    baseSgOptions
                      { sgTargets = [FileTarget $ takeFileName testFiles._linkChain1_md]
                      }
            let args = defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [takeFileName testFiles._linkChain1_md]

        it "Relative directory traversals are resolved and simplified" $
          \config -> do
            let usesDirectoryTraversal = "subdir/uses-directory-traversal.md"
            config.createDoc usesDirectoryTraversal "[parent](../parent.md)"
            let command =
                  Subgraph $
                    baseSgOptions
                      { sgTargets = [FileTarget $ usesDirectoryTraversal]
                      }
            let args = config.defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [usesDirectoryTraversal]

        it "Convoluted directory traversals are resolved and simplified" $
          \config -> do
            let usesConvolutedDirectoryTraversal = "subdir/uses-convoluted-directory-traversal.md"
            config.createDoc usesConvolutedDirectoryTraversal "[Convoluted relative directory traversal](../subdir/../parent.md)"
            let command =
                  Subgraph $
                    baseSgOptions
                      { sgTargets = [FileTarget $ usesConvolutedDirectoryTraversal]
                      }
            let args = config.defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [usesConvolutedDirectoryTraversal]

    specSetup $
      describe "Orphans" $ do
        it "Files with no links to or from them are identified as orphans" $
          \config -> do
            let orphanFile = "orphan.md"
            config.createDoc orphanFile "annie or oliver"
            let args = config.defaultArgs {argCommand = Command.Orphans}
            mdGraph args >>= outputContains [orphanFile]

        it "Orphan files are not identified as unreachable" $
          \config -> do
            let orphanFile = "orphan.md"
            config.createDoc orphanFile "annie or oliver"
            let args = config.defaultArgs {argCommand = Command.Unreachable}
            mdGraph args >>= outputDoesNotContain [orphanFile]

    specSetup $
      describe "Unreachable" $ do
        it "Files that have links but have no links to them are identified as unreachable" $
          \config -> do
            let unreachable = "unreachable.md"
            config.createDoc unreachable "[parent](./parent.md)"
            let args = config.defaultArgs {argCommand = Command.Unreachable}
            mdGraph args >>= outputContains [unreachable]

    specSetup $
      describe "Nonexistent" $ do
        it "Nonexistent returns links that do not resolve to a file" $
          \config -> do
            let unreachable = "nonexistent.md"
            let doesNotExist = "does not exist.md"
            config.createDoc unreachable . Text.pack $ "[this link does not exist](./" ++ doesNotExist ++ ")"
            let args = config.defaultArgs {argCommand = Command.Nonexes}
            mdGraph args >>= outputContains [doesNotExist]

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

          -- Scan linkChain1
          let scanOpt = ScanSome [config.testFiles._linkChain1_md]
          let args' = config.defaultArgs {argCommand = Populate, argScan = scanOpt}
          _ <- mdGraph args'

          -- Get the forwardLinks of linkChain1_md, it should contain linkChain2_md, but no others.
          rawQueryResults <- runSqlite config.dbPath $ forwardLinks Constants.linkChain1_md
          let queryResultPaths = fmap documentPath $ entityVal <$> rights rawQueryResults
          queryResultPaths `shouldContain` [Constants.linkChain2_md]
          queryResultPaths `shouldNotContain` [Constants.linkChain3_md]

      it "User can choose not to scan any files and results will be returned from the database" $
        \config -> do
          -- Populate all
          let args = config.defaultArgs {argCommand = Populate}
          mdGraph args

          -- Update a file
          config.createDoc config.testFiles._linkChain1_md "This file no longer links to anything"

          -- The existing database contents should be returned
          let command = Subgraph $ baseSgOptions {sgTargets = [FileTarget config.testFiles._linkChain1_md]}
          let args = config.defaultArgs {argCommand = command, argScan = ScanNone}
          mdGraph args >>= outputContains [takeFileName config.testFiles._linkChain2_md]
