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
          \env -> do
            let usesDirectoryTraversal = "subdir/uses-directory-traversal.md"
            env.createDoc usesDirectoryTraversal "[parent](../parent.md)"
            let command =
                  Subgraph $
                    baseSgOptions
                      { sgTargets = [FileTarget $ usesDirectoryTraversal]
                      }
            let args = env.defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [usesDirectoryTraversal]

        it "Convoluted directory traversals are resolved and simplified" $
          \env -> do
            let usesConvolutedDirectoryTraversal = "subdir/uses-convoluted-directory-traversal.md"
            env.createDoc usesConvolutedDirectoryTraversal "[Convoluted relative directory traversal](../subdir/../parent.md)"
            let command =
                  Subgraph $
                    baseSgOptions
                      { sgTargets = [FileTarget $ usesConvolutedDirectoryTraversal]
                      }
            let args = env.defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [usesConvolutedDirectoryTraversal]

    specSetup $
      describe "Orphans" $ do
        it "Files with no links to or from them are identified as orphans" $
          \env -> do
            let orphanFile = "orphan.md"
            env.createDoc orphanFile "annie or oliver"
            let args = env.defaultArgs {argCommand = Command.Orphans}
            mdGraph args >>= outputContains [orphanFile]

        it "Orphan files are not identified as unreachable" $
          \env -> do
            let orphanFile = "orphan.md"
            env.createDoc orphanFile "annie or oliver"
            let args = env.defaultArgs {argCommand = Command.Unreachable}
            mdGraph args >>= outputDoesNotContain [orphanFile]

    specSetup $
      describe "Unreachable" $ do
        it "Files that have links but have no links to them are identified as unreachable" $
          \env -> do
            let unreachable = "unreachable.md"
            env.createDoc unreachable "[parent](./parent.md)"
            let args = env.defaultArgs {argCommand = Command.Unreachable}
            mdGraph args >>= outputContains [unreachable]

    specSetup $
      describe "Nonexistent" $ do
        it "Nonexistent returns links that do not resolve to a file" $
          \env -> do
            let unreachable = "nonexistent.md"
            let doesNotExist = "does not exist.md"
            env.createDoc unreachable . Text.pack $ "[this link does not exist](./" ++ doesNotExist ++ ")"
            let args = env.defaultArgs {argCommand = Command.Nonexes}
            mdGraph args >>= outputContains [doesNotExist]

populateSpec :: Spec
populateSpec = do
  specSetup $
    describe "Scan Options" $ do
      it "User can specify specific files to scan" $
        \env -> do
          -- Populate only linkChain2_md
          let scanOpt = ScanSome [env.testFiles._linkChain2_md]
          let args' = env.defaultArgs {argCommand = Populate, argScan = scanOpt}
          mdGraph args'

          -- It should be the only document in the database, even though it has forward and backward links and there are more in the library
          rawQueryResults <- runSqlite env.dbPath $ getAllDocuments
          let dbDocuments = fmap documentPath $ entityVal <$> rawQueryResults
          dbDocuments `shouldContain` [Constants.linkChain2_md]
          dbDocuments `shouldNotContain` [Constants.linkChain1_md]

          -- Scan linkChain1
          let scanOpt = ScanSome [env.testFiles._linkChain1_md]
          let args' = env.defaultArgs {argCommand = Populate, argScan = scanOpt}
          _ <- mdGraph args'

          -- Get the forwardLinks of linkChain1_md, it should contain linkChain2_md, but no others.
          rawQueryResults <- runSqlite env.dbPath $ forwardLinks Constants.linkChain1_md
          let queryResultPaths = fmap documentPath $ entityVal <$> rights rawQueryResults
          queryResultPaths `shouldContain` [Constants.linkChain2_md]
          queryResultPaths `shouldNotContain` [Constants.linkChain3_md]

      it "User can choose not to scan any files and results will be returned from the database" $
        \env -> do
          -- Populate all
          let args = env.defaultArgs {argCommand = Populate}
          mdGraph args

          -- Update a file
          env.createDoc env.testFiles._linkChain1_md "This file no longer links to anything"

          -- The existing database contents should be returned
          let command = Subgraph $ baseSgOptions {sgTargets = [FileTarget env.testFiles._linkChain1_md]}
          let args = env.defaultArgs {argCommand = command, argScan = ScanNone}
          mdGraph args >>= outputContains [takeFileName env.testFiles._linkChain2_md]
