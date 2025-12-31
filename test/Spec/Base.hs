{-# LANGUAGE StrictData #-}

module Spec.Base where

import qualified Constants
import Control.Exception.Base
import Control.Monad
import qualified Control.Monad as Monad
import qualified Control.Monad as Traversable
import Control.Monad.Reader
import Data.Either as Either
import qualified Data.List as List
import Data.String (IsString)
import Data.Text as Text
import Data.Text.IO as Text
import MdGraph.App.Arguments
import qualified MdGraph.App.Command as Command
import qualified MdGraph.App.LogLevel as LogLevel
import qualified MdGraph.TagDirection as TagDirection
import System.Directory
import System.FilePath
import System.IO
import qualified System.IO
import System.Random
import Test.Hspec

-- | Add an extension only if one doesn't already exist
(+<.>) :: FilePath -> FilePath -> FilePath
path +<.> extension = if hasExtension path then path else path <.> extension

data SetupConfig = SetupConfig
  {testLibraryDir :: FilePath}
  deriving (Show)

newtype TestSetup a = TestSetup
  {runTestSetup :: ReaderT SetupConfig IO a}
  deriving (Monad, Functor, Applicative, MonadReader SetupConfig, MonadIO)

_runSetup :: SetupConfig -> TestSetup a -> IO a
_runSetup config i = runReaderT (runTestSetup $ i) config

setupTestLibrary :: IO ()
setupTestLibrary = do
  config <- mkSetupConfig
  _runSetup config $ do
    putDoc "angle-brackets" ["[Links to parent](<./parent.md> \"this is hint text\")"]
    putDoc "has-nonexistent-link" ["[This link is broken and goes nowhere](./link-to-nonexistent-file.md)"]
    putDoc
      "has-static-file-link"
      [ "[This is a link to static file](./static.txt)",
        "",
        "[This is a link to a static file in the subdir](./subdir/static2.txt)"
      ]
    putDoc "link chain 1" ["[forward to link 2](./link chain 2.md)"]
    putDoc "link chain 2" ["[forward to link 3](./link chain 3.md)"]
    putDoc "link chain 3" ["[forward to link 4](./link chain 4.md)"]
    putDoc "link chain 4" ["This file Just exists"]
    putDoc "links-dont-have-extensions" ["[This link doesn't have an extension](./parent)"]
    putDoc "parent" ["Parent"]
    putDoc "orphan" ["This file has no links, and no files link to it"]
    putDoc "unreachable" ["This file has no links to it, but links to [parent](./parent.md)"]
    putDoc "static.txt" ["Pretend this is a static file, like an image"]
    putDoc "subdir/static2.txt" ["Pretend this is a static file, like an image"]
    putDoc "subdir/uses convoluted directory traversal" ["[Convoluted relative directory traversal](../subdir/../parent.md)"]
    putDoc
      "subdir/uses directory traversal"
      [ "[Relative directory traversal](../parent.md)",
        "",
        "[Static directory traversal](../static.txt)"
      ]

putLines :: FilePath -> [Text] -> IO ()
putLines path lines = do
  withFile path WriteMode $ \handle ->
    Monad.mapM_ (Text.hPutStr handle) lines

putDoc :: FilePath -> [Text] -> TestSetup ()
putDoc docName lines = do
  libDir <- asks testLibraryDir
  let docPath = libDir </> docName +<.> "md"
  liftIO $ putLines docPath lines

createTempLibraryDir :: IO FilePath
createTempLibraryDir = do
  systemTempDir <- getTemporaryDirectory
  rand <- randomRIO (100000, 999999) :: IO Int
  let dirName = "md-graph-test-lib-" ++ show rand
  let testLibPath = systemTempDir </> dirName
  createDirectory testLibPath
  createDirectory $ testLibPath </> "subdir"
  return testLibPath

mkSetupConfig :: IO SetupConfig
mkSetupConfig = do
  libDir <- createTempLibraryDir
  return $ SetupConfig libDir

getLibraryDir :: IO FilePath
getLibraryDir = do
  curDir <- getCurrentDirectory
  return $ curDir </> "test" </> "library"

defaultSpecArgs :: Arguments
defaultSpecArgs =
  Arguments
    { argLibrary = "./test/library",
      argDefExt = "md",
      argDatabase = DbFile ":memory:",
      argLogLevel = LogLevel.None,
      argCommand = Command.Populate,
      argScan = ScanAll
    }

shouldReturnFrom :: (HasCallStack, Show a, Eq a) => a -> IO a -> Expectation
shouldReturnFrom = flip shouldReturn

outputContains :: (HasCallStack, Show a, Show r, Eq r, Ord r) => [r] -> Either a [r] -> Expectation
outputContains expected eitherList = do
  let orderedExpected = List.sort expected
  let orderedEither = List.sort <$> eitherList
  let liftedExpectation = (`shouldContain` orderedExpected) <$> orderedEither
  fromRight (shouldSatisfy eitherList isRight) liftedExpectation

outputDoesNotContain :: (HasCallStack, Show a, Show r, Eq r, Ord r) => [r] -> Either a [r] -> Expectation
outputDoesNotContain rejectedValues eitherList = do
  let failIfLeft _ = eitherList `shouldSatisfy` isRight
  either failIfLeft shouldNotContainAnyRejectedValues eitherList
  where
    shouldNotContainAnyRejectedValues actualValues = Monad.forM_ rejectedValues $ \value -> actualValues `shouldNotContain` [value]

withTempDbFile :: (Arguments -> IO a) -> IO a
withTempDbFile fn = do
  tempDir <- getTemporaryDirectory
  let acquire =
        do
          (path, handle) <- System.IO.openTempFileWithDefaultPermissions tempDir "mdgraph.db"
          Text.putStrLn . Text.unwords $ ["Using temp db file", Text.pack path]
          System.IO.hClose handle -- we don't actually need the handle, we Just want the path
          return path
  let run tempDbPath =
        let argsWithTempDb = defaultSpecArgs {argDatabase = DbFile . Text.pack $ tempDbPath}
         in fn argsWithTempDb
  let release tempDbPath =
        do
          System.Directory.removeFile tempDbPath
          return ()

  bracket
    acquire
    release
    run
