{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
import qualified Constants
import Control.Exception (evaluate)
import qualified Control.Exception as E
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Data.Either (fromRight, isRight)
import qualified Data.Text as T
import Database.Persist.Sqlite (runSqlPersistM, wrapConnection)
import Database.Sqlite (open)
import qualified FilesSpec
import MdGraph (mdGraph)
import qualified MdGraph
import MdGraph.App (App (runApp), Env (Env))
import MdGraph.App.Arguments (Arguments (..))
import qualified MdGraph.App.Arguments as Arguments
import MdGraph.App.Command (BacklinkOptions (..), Command (..), SubgraphOptions (..), SubgraphTarget (..))
import qualified MdGraph.App.Command as Command
import qualified MdGraph.App.LogLevel as LogLevel
import MdGraph.App.RunCommand (runCommand)
import MdGraph.Config (Config (Config, libraryPath))
import MdGraph.File (Files (..), isAncestorOf, normaliseEvil, unrelativize)
import MdGraph.File.Internal (AbsolutePath (..), File)
import MdGraph.Node (Link (..))
import qualified MdGraph.TagDirection as TagDirection
import Spec.Base
import System.Directory (getCurrentDirectory)
import System.FilePath
import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.Hspec.QuickCheck
import Prelude

data FakeFiles = FakeFiles
  { ffTrueAbsolutePath :: FilePath -> FilePath,
    ffMaybeFile :: FilePath -> Maybe FilePath,
    ffFindDocuments :: [File]
  }

newtype FilesMock a = FilesMock {runFilesMock :: ReaderT FakeFiles Identity a}
  deriving (Monad, Functor, Applicative, MonadReader FakeFiles)

instance Files FilesMock where
  trueAbsolutePath a = asks ffTrueAbsolutePath <*> pure a
  maybeFile a = asks ffMaybeFile <*> pure a
  findDocuments = asks ffFindDocuments

filesMock :: FakeFiles
filesMock = FakeFiles id Just []

main :: IO ()
main = do
  libraryDir <- getLibraryDir
  hspec FilesSpec.spec
  hspec $ do
    describe "Backlinks" $ do
      it "Correct backlinks are returned" $ do
        let command = Backlinks (BacklinkOptions [FileTarget $ libraryDir </> Constants.linkChain4_md] 2)
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args `shouldReturn` Right [Constants.linkChain3_md, Constants.linkChain4_md]

    describe "Subgraph" $ do
      it "Return the full subgraph of a file" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ libraryDir </> Constants.linkChain1_md],
                    sgInclNonex = False,
                    sgInclStatic = False,
                    sgTagDir = TagDirection.In,
                    sgDepth = -1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain1_md, Constants.linkChain2_md, Constants.linkChain3_md, Constants.linkChain4_md]

      it "Nonexistent (broken) links are included if requested" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ libraryDir </> Constants.hasNonExistentLink_md],
                    sgInclNonex = True,
                    sgInclStatic = False,
                    sgTagDir = TagDirection.In,
                    sgDepth = -1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args >>= outputContains ["link-to-nonexistent-file.md"]

      it "Nonexistent (broken) links are _not_ included if not requested" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ libraryDir </> Constants.hasNonExistentLink_md],
                    sgInclNonex = False,
                    sgInclStatic = False,
                    sgTagDir = TagDirection.In,
                    sgDepth = -1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args >>= outputDoesNotContain ["link-to-nonexistent-file.md"]

      it "Static files (ie files not recognized as documents) are included if requested" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ libraryDir </> Constants.hasStaticFileLink_md],
                    sgInclNonex = False,
                    sgInclStatic = True,
                    sgTagDir = TagDirection.In,
                    sgDepth = -1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.static_txt]

      it "Static files (ie files not recognized as documents) are _not_ included if not requested" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ libraryDir </> Constants.hasStaticFileLink_md],
                    sgInclNonex = False,
                    sgInclStatic = False,
                    sgTagDir = TagDirection.In,
                    sgDepth = -1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args >>= outputDoesNotContain [Constants.static_txt]

    describe "Path handling" $ do
      it "Absolute paths are accepted and relativized to the library" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ libraryDir </> Constants.linkChain1_md],
                    sgInclNonex = True,
                    sgInclStatic = True,
                    sgTagDir = TagDirection.In,
                    sgDepth = 1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain1_md]

      it "Paths relative to the current directory are accepted and relativized to the library" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ "./test/library" </> Constants.linkChain1_md],
                    sgInclNonex = True,
                    sgInclStatic = True,
                    sgTagDir = TagDirection.In,
                    sgDepth = 1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.linkChain1_md]

      it "Relative directory traversals are resolved and simplified" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ libraryDir </> Constants.usesDirectoryTraversal_md],
                    sgInclNonex = True,
                    sgInclStatic = True,
                    sgTagDir = TagDirection.In,
                    sgDepth = -1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args `shouldReturn` Right [Constants.usesDirectoryTraversal_md, Constants.parent_md]

      it "Convoluted directory traversals are resolved and simplified" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ libraryDir </> Constants.usesConvolutedDirectoryTraversal_md],
                    sgInclNonex = True,
                    sgInclStatic = True,
                    sgTagDir = TagDirection.In,
                    sgDepth = -1
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
                    sgDepth = -1
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
                    sgDepth = -1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args >>= outputContains [Constants.parent_md]
