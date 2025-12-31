{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import qualified Constants
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

    let baseSgOptions =
          SubgraphOptions
            { sgInclNonex = False,
              sgInclStatic = False,
              sgTagDir = TagDirection.In,
              sgMaxDepth = subgraphDefaultMaxDepth,
              sgTargets = [],
              sgMinDepth = subgraphDefaultMinDepth
            }
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

    describe "Orphans" $ do
      it "Files with no links to or from them are identified" $ do
        let args = defaultSpecArgs {argCommand = Command.Orphans}
        mdGraph args >>= outputContains [Constants.orphan_md]

      it "Orphan files are not identified as unreachable" $ do
        let args = defaultSpecArgs {argCommand = Command.Unreachable}
        mdGraph args >>= outputDoesNotContain [Constants.orphan_md]

    describe "Unreachable" $ do
      it "Files that have links but have no links to them are identified" $ do
        let args = defaultSpecArgs {argCommand = Command.Unreachable}
        mdGraph args >>= outputContains [Constants.unreachable_md]

    describe "Nonexistent" $ do
      it "Files" $ do
        let args = defaultSpecArgs {argCommand = Command.Nonexes}
        mdGraph args >>= outputContains ["link-to-nonexistent-file.md"]

    describe "Parsing" $ do
      it "File extensions in links may be omitted and the default is used instead" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ libraryDir </> Constants.linksDontHaveExtensions_md],
                    sgInclNonex = True,
                    sgInclStatic = True,
                    sgTagDir = TagDirection.In,
                    sgMaxDepth = -1,
                    sgMinDepth = -1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.parent_md]

      it "Links may use angle brackets and hint text" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ libraryDir </> Constants.angleBrackets_md],
                    sgInclNonex = True,
                    sgInclStatic = True,
                    sgTagDir = TagDirection.In,
                    sgMaxDepth = -1,
                    sgMinDepth = -1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.parent_md]

populateSpec :: Spec
populateSpec = do
  describe "Scan Options" $ do
    it "User can specify specific files to scan" $ do
      libraryDir <- getLibraryDir
      withTempDbFile $ \args -> do
        let dbPath = dbFile $ argDatabase args

        -- Populate only linkChain2_md
        let scanOpt = ScanSome [libraryDir </> Constants.linkChain2_md]
        let args' = args {argCommand = Populate, argScan = scanOpt}
        mdGraph args'

        -- It should be the only document in the database, even though it has forward and backward links and there are more in the library
        rawQueryResults <- runSqlite dbPath $ getAllDocuments
        let dbDocuments = fmap documentPath $ entityVal <$> rawQueryResults
        dbDocuments `shouldContain` [Constants.linkChain2_md]
        dbDocuments `shouldNotContain` [Constants.linkChain1_md]

        -- Populate linkChain1
        let scanOpt = ScanSome [libraryDir </> Constants.linkChain1_md]
        let args' = args {argCommand = Populate, argScan = scanOpt}
        _ <- mdGraph args'

        -- Get the forwardLinks of linkChain1_md, it should contain linkChain2_md, but no others.
        rawQueryResults <- runSqlite dbPath $ forwardLinks Constants.linkChain1_md
        let queryResultPaths = fmap documentPath $ entityVal <$> rights rawQueryResults
        queryResultPaths `shouldContain` [Constants.linkChain2_md]
        queryResultPaths `shouldNotContain` [Constants.linkChain3_md]
