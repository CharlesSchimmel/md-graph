{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Spec.Base where

import Constants
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

data SpecConfig = SpecConfig
  { specLibraryDir :: FilePath,
    testFiles :: TestFiles,
    defaultArgs :: Arguments,
    dbPath :: Text
  }
  deriving (Show)

specSetup :: SpecWith SpecConfig -> Spec
specSetup = around withTempLibrary

withTempLibrary :: (SpecConfig -> IO ()) -> IO ()
withTempLibrary action = bracket setupTestLibrary (const $ return ()) $ \config ->
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    System.IO.putStrLn $ "Using temp library dir: " ++ config.specLibraryDir
    setCurrentDirectory config.specLibraryDir
    action config

setupTestLibrary :: IO SpecConfig
setupTestLibrary = do
  config <- mkSetupConfig
  let putDoc' = putDoc config.specLibraryDir
  putDoc' "angle-brackets" ["[Links to parent](<./parent.md> \"this is hint text\")"]
  putDoc' "has-nonexistent-link" ["[This link is broken and goes nowhere](./link-to-nonexistent-file.md)"]
  putDoc'
    "has-static-file-link"
    [ "[This is a link to static file](./static.txt)",
      "",
      "[This is a link to a static file in the subdir](./subdir/static2.txt)"
    ]
  putDoc' "link chain 1" ["[forward to link 2](./link chain 2.md)"]
  putDoc' "link chain 2" ["[forward to link 3](./link chain 3.md)"]
  putDoc' "link chain 3" ["[forward to link 4](./link chain 4.md)"]
  putDoc' "link chain 4" ["This file Just exists"]
  putDoc' "links-dont-have-extensions" ["[This link doesn't have an extension](./parent)"]
  putDoc' "parent" ["Parent"]
  putDoc' "orphan" ["This file has no links, and no files link to it"]
  putDoc' "unreachable" ["This file has no links to it, but links to [parent](./parent.md)"]
  putDoc' "static.txt" ["Pretend this is a static file, like an image"]
  putDoc' "subdir/static2.txt" ["Pretend this is a static file, like an image"]
  putDoc' "subdir/uses convoluted directory traversal" ["[Convoluted relative directory traversal](../subdir/../parent.md)"]
  putDoc'
    "subdir/uses directory traversal"
    [ "[Relative directory traversal](../parent.md)",
      "",
      "[Static directory traversal](../static.txt)"
    ]
  return config

putDoc :: FilePath -> FilePath -> [Text] -> IO ()
putDoc libDir docName lines = do
  let docPath = libDir </> docName +<.> "md"
  withFile docPath WriteMode $ \handle ->
    Monad.mapM_ (Text.hPutStr handle) lines

createTempLibraryDir :: IO FilePath
createTempLibraryDir = do
  systemTempDir <- getTemporaryDirectory
  rand <- randomRIO (1000000, 9999999) :: IO Int
  let randomLibDirName = "library-" ++ show rand
  let testLibPath = systemTempDir </> "md-graph-test" </> randomLibDirName
  createDirectoryIfMissing True testLibPath
  createDirectory $ testLibPath </> "subdir"
  return testLibPath

mkSetupConfig :: IO SpecConfig
mkSetupConfig = do
  libDir <- createTempLibraryDir

  let dbFile = Text.pack $ libDir </> "md-graph.db"
  let args =
        defaultSpecArgs
          { argLibrary = libDir,
            argDatabase = DbFile dbFile
          }
  return $
    SpecConfig
      { specLibraryDir = libDir,
        testFiles = Constants.testFiles libDir,
        defaultArgs = args,
        dbPath = dbFile
      }

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
