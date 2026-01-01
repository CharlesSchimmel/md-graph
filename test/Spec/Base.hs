{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Spec.Base (SpecEnv (..), specSetup, baseSgOptions, outputContains, outputDoesNotContain, mkLink) where

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
import MdGraph.App.Command
import qualified MdGraph.App.Command as Command
import qualified MdGraph.App.LogLevel as LogLevel
import qualified MdGraph.TagDirection as TagDirection
import System.Directory
import System.FilePath
import System.IO
import qualified System.IO
import System.Random
import Test.Hspec
import Text.Printf

-- | Add an extension only if one doesn't already exist
(+<.>) :: FilePath -> FilePath -> FilePath
path +<.> extension = if hasExtension path then path else path <.> extension

data SpecEnv = SpecEnv
  { specLibraryDir :: FilePath,
    testFiles :: TestFiles,
    defaultArgs :: Arguments,
    dbPath :: Text,
    -- | Create a document in the library, relative to the library. Returns the document's absolute filepath
    createDoc :: FilePath -> Text -> IO FilePath
  }

specSetup :: SpecWith SpecEnv -> Spec
specSetup = around withTempLibrary

withTempLibrary :: (SpecEnv -> IO ()) -> IO ()
withTempLibrary action = do
  config <- setupTestLibrary -- Could tear it down but idk I don't like to delete things off the filesystem.
  let logLibraryDirOnTestFailure = onException (action config) (logTestLibrary config)
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory config.specLibraryDir
    logLibraryDirOnTestFailure
  where
    logTestLibrary :: SpecEnv -> IO ()
    logTestLibrary config = printf "Used temp library dir: %s\n" config.specLibraryDir

setupTestLibrary :: IO SpecEnv
setupTestLibrary = do
  config <- mkSpecEnv
  let putDoc' = putDoc config.specLibraryDir
  putDoc' "link chain 1" ["[forward to link 2](./link chain 2.md)"]
  putDoc' "link chain 2" ["[forward to link 3](./link chain 3.md)"]
  putDoc' "link chain 3" ["[forward to link 4](./link chain 4.md)"]
  putDoc' "link chain 4" ["This file just exists"]
  putDoc' "parent" ["Parent"]
  putDoc'
    "subdir/uses directory traversal"
    [ "[Relative directory traversal](../parent.md)",
      "",
      "[Static directory traversal](../static.txt)"
    ]
  return config

putDoc :: FilePath -> FilePath -> [Text] -> IO FilePath
putDoc libDir docName lines = do
  let docPath = libDir </> docName +<.> "md"
  withFile docPath WriteMode $ \handle ->
    Monad.mapM_ (Text.hPutStr handle) lines
  return docPath

createTempLibraryDir :: IO FilePath
createTempLibraryDir = do
  systemTempDir <- getTemporaryDirectory
  rand <- randomRIO (1000000, 9999999) :: IO Int
  let randomLibDirName = "library-" ++ show rand
  let testLibPath = systemTempDir </> "md-graph-test" </> randomLibDirName
  createDirectoryIfMissing True testLibPath
  createDirectory $ testLibPath </> "subdir"
  return testLibPath

mkSpecEnv :: IO SpecEnv
mkSpecEnv = do
  libDir <- createTempLibraryDir

  let dbFile = Text.pack $ libDir </> "md-graph.db"
  let args =
        defaultSpecArgs
          { argLibrary = libDir,
            argDatabase = DbFile dbFile
          }
  return $
    SpecEnv
      { specLibraryDir = libDir,
        testFiles = Constants.testFiles libDir,
        defaultArgs = args,
        dbPath = dbFile,
        createDoc = \path content -> putDoc libDir path [content]
      }

-- | Build a markdown link of the form [linkText)(path)
mkLink :: Text -> FilePath -> Text
mkLink linkText path = Text.concat ["[", linkText, "](./", Text.pack path, ")"]

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

baseSgOptions :: SubgraphOptions
baseSgOptions =
  SubgraphOptions
    { sgInclNonex = False,
      sgInclStatic = False,
      sgTagDir = TagDirection.In,
      sgMaxDepth = subgraphDefaultMaxDepth,
      sgTargets = [],
      sgMinDepth = subgraphDefaultMinDepth
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
