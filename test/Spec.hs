{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import MdGraph.Persist.Query (forwardLinks, getAllDocuments, getAllEdges, getAllTags, orphansM, unreachableM)
import MdGraph.Persist.Schema (Document (documentPath), Edge (edgeLabel), EntityField (..), Tag (tagName))
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
    parseSpec

    setupSpecEnv $
      describe "Path handling" $ do
        it "Absolute paths are accepted and relativized to the library" $
          \SpecEnv {..} -> do
            let command =
                  Subgraph $
                    baseSgOptions
                      { sgTargets = [FileTarget $ testFiles.linkChain1.absolute],
                        sgMaxDepth = 1
                      }
            let args = defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [testFiles.linkChain1.pathFromLibrary]

        it "Paths relative to the current directory are accepted and relativized to the library" $
          \SpecEnv {..} -> do
            pathFromCurrent <- testFiles.linkChain1.pathFromCurrent
            let command =
                  Subgraph $
                    baseSgOptions
                      { sgTargets = [FileTarget $ pathFromCurrent]
                      }
            let args = defaultArgs {argCommand = command}
            mdGraph args >>= outputContains [testFiles.linkChain1.pathFromLibrary]

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

    setupSpecEnv $
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

    setupSpecEnv $
      describe "Unreachable" $ do
        it "Files that have links but have no links to them are identified as unreachable" $
          \env -> do
            let unreachable = "unreachable.md"
            env.createDoc unreachable "[parent](./parent.md)"
            let args = env.defaultArgs {argCommand = Command.Unreachable}
            mdGraph args >>= outputContains [unreachable]

    setupSpecEnv $
      describe "Nonexistent" $ do
        it "Nonexistent returns links that do not resolve to a file" $
          \env -> do
            let unreachable = "nonexistent.md"
            let doesNotExist = "does not exist.md"
            env.createDoc unreachable . Text.pack $ "[this link does not exist](./" ++ doesNotExist ++ ")"
            let args = env.defaultArgs {argCommand = Command.Nonexes}
            mdGraph args >>= outputContains [doesNotExist]

then__ = id

populateSpec :: Spec
populateSpec = do
  setupSpecEnv $
    describe "Scan Options" $ do
      it "User can specify specific files to scan" $
        \env -> do
          -- Populate only linkChain2_md
          let scanOpt = ScanSome [env.testFiles.linkChain2.absolute]
          let args' = env.defaultArgs {argCommand = Populate, argScan = scanOpt}
          mdGraph args'

          -- It should be the only document in the database, even though it has forward and backward links and there are more in the library
          rawQueryResults <- runSqlite env.dbPath $ getAllDocuments
          let dbDocuments = fmap documentPath $ entityVal <$> rawQueryResults
          step "link-chain-2 is the only document in the database" $
            dbDocuments `shouldBe` [env.testFiles.linkChain2.pathFromLibrary]
          step "No other documents are in the database" $
            dbDocuments `shouldNotContain` [env.testFiles.linkChain1.pathFromLibrary]

          -- Scan only linkChain1
          let scanOpt = ScanSome [env.testFiles.linkChain1.absolute]
          let args' = env.defaultArgs {argCommand = Populate, argScan = scanOpt}
          mdGraph args'

          -- Get the forwardLinks of linkChain1_md, it should contain linkChain2_md, but no others.
          rawQueryResults <- runSqlite env.dbPath $ forwardLinks env.testFiles.linkChain1.pathFromLibrary
          let queryResultPaths = fmap documentPath $ entityVal <$> rights rawQueryResults
          step "Forward links from link-chain-1 are in the database" $
            queryResultPaths `shouldBe` [env.testFiles.linkChain2.pathFromLibrary]

          step "Forward links from link-chain-2 are not returned" $
            queryResultPaths `shouldNotContain` [env.testFiles.linkChain3.pathFromLibrary]

      it "User can choose not to scan any files and results will be returned from the database" $
        \env -> do
          -- Populate all
          let args = env.defaultArgs {argCommand = Populate}
          mdGraph args

          -- Update a file
          env.createDoc env.testFiles.linkChain1.absolute "This file no longer links to anything"

          -- The existing database contents should be returned
          let command = Subgraph $ baseSgOptions {sgTargets = [FileTarget env.testFiles.linkChain1.absolute]}
          let args = env.defaultArgs {argCommand = command, argScan = ScanNone}
          mdGraph args >>= outputContains [takeFileName env.testFiles.linkChain2.absolute]

parseSpec :: Spec
parseSpec = do
  setupSpecEnv $
    describe "Parse" $ do
      it "Link labels are parsed and stored" $
        \env -> do
          let docName = "has-link-text.md"
          let linkLabel = "this is the link label"
          env.createDoc docName $ Text.pack $ "[" ++ linkLabel ++ "](./link target is not relevant.md)"

          let scanOpt = ScanSome [docName]
          let args = env.defaultArgs {argCommand = Populate, argScan = scanOpt}
          mdGraph args

          rawQueryResults <- runSqlite env.dbPath getAllEdges
          let queryResultPaths = edgeLabel . entityVal <$> rawQueryResults
          step "Simple link labels are parsed and stored" $
            queryResultPaths `shouldBe` [linkLabel]
      it "Tags are parsed and stored" $
        \env -> do
          let docName = "has-tags.md"
          let metaTagText = "meta-tag"
          let documentFrontmatter = "---\ntitle: foo bar baz\ntags: " ++ metaTagText ++ "\ndate: 2000-01-01\n---"
          let inlineTagText = "inline-tag"
          let documentContent = unlines [documentFrontmatter, "Here is some text and then the tag #" ++ inlineTagText]

          env.createDoc docName $ Text.pack documentContent

          let scanOpt = ScanSome [docName]
          let args = env.defaultArgs {argCommand = Populate, argScan = scanOpt}
          mdGraph args

          rawQueryResults <- runSqlite env.dbPath getAllTags
          let queryResultPaths = tagName . entityVal <$> rawQueryResults
          step "Meta tags are parsed and stored" $
            queryResultPaths `shouldContain` [metaTagText]
          step "Inline tags are parsed and stored" $
            queryResultPaths `shouldContain` [inlineTagText]
