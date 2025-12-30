{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
import Aux.Common
import qualified Constants
import Control.Exception
import Control.Monad
import Data.Either
import Data.Function ((&))
import Data.Text as Text
import qualified Data.Text.IO as Text
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
import MdGraph.Persist.Query (forwardLinks)
import MdGraph.Persist.Schema (Document (documentPath))
import qualified MdGraph.TagDirection as TagDirection
import Spec.Base
import qualified SubgraphSpec
import System.Directory (getCurrentDirectory, getTemporaryDirectory, removeFile)
import System.FilePath
import System.IO
import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.Hspec.QuickCheck
import Prelude

main :: IO ()
main = do
  libraryDir <- getLibraryDir
  hspec FilesSpec.spec
  hspec $ SubgraphSpec.spec libraryDir
  hspec $ populateSpec
  hspec $ do
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
  describe "Populate" $ do
    it "Populate" $ do
      libraryDir <- getLibraryDir
      withTempDb $ \args -> do
        -- let args = defaultSpecArgs {argLogLevel = LogLevel.Debug}
        let dbPath = dbFile $ argDatabase args
        let fileUnderTest = libraryDir </> Constants.linkChain1_md
        let command = Populate $ PopulateTargets {popTargets = [fileUnderTest]}
        let args' = args {argCommand = command, argLogLevel = LogLevel.Debug}
        _ <- mdGraph args'

        rawQueryResults <- runSqlite dbPath $ forwardLinks Constants.linkChain1_md
        let queryResultPaths = fmap documentPath $ entityVal <$> rights rawQueryResults
        queryResultPaths `shouldContain` [Constants.linkChain1_md]

        return ()

withTempDb :: (Arguments -> IO a) -> IO a
withTempDb fn = do
  tempDir <- getTemporaryDirectory
  let acquire =
        do
          (path, handle) <- System.IO.openTempFileWithDefaultPermissions tempDir "mdgraph.db"
          Text.putStrLn . Text.unwords $ ["Using temp db file", Text.pack path]
          System.IO.hClose handle -- we don't actually need the handle, we Just want the path
          return path
  -- let acquire =
  --       do
  --         -- blahH <- System.IO.openFile "/tmp/blah.db" System.IO.ReadWriteMode
  --         -- return ("/tmp/blah.db", blahH)
  --         return ("/tmp/blah.db", ())
  let run tempDbPath =
        let argsWithTempDb = defaultSpecArgs {argDatabase = DbFile . Text.pack $ tempDbPath}
         in fn argsWithTempDb
  let release tempDbPath =
        do
          -- hClose tempDbHandle
          System.Directory.removeFile tempDbPath
          return ()
  -- acquire >>= run

  bracket
    acquire
    release
    run
