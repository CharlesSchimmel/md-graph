{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
import qualified Constants
import Control.Exception (evaluate)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Text as T
import Database.Persist.Sqlite (runSqlPersistM, wrapConnection)
import Database.Sqlite (open)
import MdGraph (mdGraph)
import MdGraph.App (App (runApp), Env (Env))
import MdGraph.App.Arguments (Arguments (..))
import qualified MdGraph.App.Arguments as Arguments
import MdGraph.App.Command (BacklinkOptions (..), Command (..), SubgraphOptions (..), SubgraphTarget (..))
import qualified MdGraph.App.Command as Command
import qualified MdGraph.App.LogLevel as LogLevel
import MdGraph.App.RunCommand (runCommand)
import MdGraph.Config (Config (Config, libraryPath))
import MdGraph.File.Internal
import qualified MdGraph.TagDirection as TagDirection
import System.Directory (getCurrentDirectory)
import System.FilePath
import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.Hspec.QuickCheck
import Prelude

main :: IO ()
main = do
  libraryDir <- getLibraryDir
  hspec $ do
    describe "Backlinks" $ do
      it "Correct backlinks are returned" $ do
        let command = Backlinks (BacklinkOptions [FileTarget $ libraryDir </> "link chain 4.md"] 2)
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args `shouldReturn` Right ["link chain 3.md", "link chain 4.md"]

    describe "Subgraph" $ do
      it "Return the full subgraph of a file" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ libraryDir </> "link chain 1.md"],
                    sgInclNonex = False,
                    sgInclStatic = False,
                    sgTagDir = TagDirection.In,
                    sgDepth = -1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args `shouldReturn` Right ["link chain 1.md", "link chain 2.md", "link chain 3.md", "link chain 4.md"]

    describe "Path handling" $ do
      it "Absolute paths are accepted and relativized to the library" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ libraryDir </> "link chain 1.md"],
                    sgInclNonex = True,
                    sgInclStatic = True,
                    sgTagDir = TagDirection.In,
                    sgDepth = 1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args >>= outputContains "link chain 1.md"

      it "Paths relative to the current directory are accepted and relativized to the library" $ do
        let command =
              Subgraph $
                SubgraphOptions
                  { sgTargets = [FileTarget $ "./test/library" </> "link chain 1.md"],
                    sgInclNonex = True,
                    sgInclStatic = True,
                    sgTagDir = TagDirection.In,
                    sgDepth = 1
                  }
        let args = defaultSpecArgs {argCommand = command}
        mdGraph args >>= outputContains "link chain 1.md"

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
        mdGraph args >>= outputContains Constants.parent_md

    describe "Orphans" $ do
      it "Files with no links to or from them are identified" $ do
        let args = defaultSpecArgs {argCommand = Command.Orphans}
        mdGraph args >>= outputContains Constants.orphan_md

      it "Orphan files are not identified as unreachable" $ do
        let args = defaultSpecArgs {argCommand = Command.Unreachable}
        mdGraph args >>= outputDoesNotContains Constants.orphan_md

    describe "Unreachable" $ do
      it "Files that have links but have no links to them are identified" $ do
        let args = defaultSpecArgs {argCommand = Command.Unreachable}
        mdGraph args >>= outputContains Constants.unreachable_md

    describe "Nonexistant" $ do
      it "Files" $ do
        let args = defaultSpecArgs {argCommand = Command.Nonexes}
        mdGraph args >>= outputContains "this-goes-nowhere.md"

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
        mdGraph args >>= outputContains Constants.parent_md

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
        mdGraph args >>= outputContains Constants.parent_md

getLibraryDir :: IO FilePath
getLibraryDir = do
  curDir <- getCurrentDirectory
  return $ curDir </> "test" </> "library"

defaultSpecArgs :: Arguments
defaultSpecArgs =
  Arguments
    { argLibrary = "./test/library",
      argDefExt = "md",
      argDatabase = Arguments.DbFile ":memory:",
      argLogLevel = LogLevel.None,
      argCommand = Command.Populate
    }

aoeu source dest = trueDest </> joinDir absoluteParts
  where
    sourceParts = splitDirectories source
    destParts = splitDirectories dest
    isRelativePart = (== "..")
    relativeParts = length . takeWhile isRelativePart $ destParts
    absoluteParts = dropWhile isRelativePart destParts
    trueDest = joinDir $ reverse . drop relativeParts . reverse $ sourceParts

shouldContainIO :: (HasCallStack, Show a, Eq a) => IO [a] -> [a] -> Expectation
action `shouldContainIO` expected = action >>= (`shouldContain` expected)

shouldReturnFrom :: (HasCallStack, Show a, Eq a) => a -> IO a -> Expectation
shouldReturnFrom = flip shouldReturn

-- -- shouldRight :: Either l r -> IO r
-- -- shouldRight e =
satisfiesRight :: (Show l, Show r) => (r -> Bool) -> Either l r -> Expectation
satisfiesRight test eith = shouldSatisfy eith $ either (const False) test

outputContains :: String -> Either T.Text [String] -> Expectation
outputContains value output = satisfiesRight (Prelude.elem value) output

outputDoesNotContains :: String -> Either T.Text [String] -> Expectation
outputDoesNotContains value output = satisfiesRight (Prelude.notElem value) output
