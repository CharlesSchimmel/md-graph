{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Spec.Base
  ( SpecEnv (..),
    setupSpecEnv,
    baseSgOptions,
    outputContains,
    outputDoesNotContain,
    mkLink,
    step,
  )
where

import Constants
import qualified Control.Exception as Exception
import Control.Exception.Base
import Control.Monad
import qualified Control.Monad as Monad
import qualified Control.Monad as Traversable
import Control.Monad.Reader
import Data.Either as Either
import qualified Data.List as List
import Data.String (IsString)
import Data.Text as Text hiding (isInfixOf)
import Data.Text.IO as Text
import MdGraph.App.Arguments
import MdGraph.App.Command
import qualified MdGraph.App.Command as Command
import qualified MdGraph.App.LogLevel as LogLevel
import MdGraph.File
import qualified MdGraph.TagDirection as TagDirection
import System.Directory
import System.FilePath
import System.IO
import qualified System.IO
import System.Random
import qualified Test.HUnit
import Test.HUnit.Lang
import Test.Hspec
import Text.Printf

data SpecEnv = SpecEnv
  { libraryDir :: FilePath,
    testFiles :: TestFiles',
    defaultArgs :: Arguments,
    dbPath :: Text,
    -- | Create a document in the library, relative to the library. Returns the document's Absolute filepath.
    createDoc ::
      -- \| The relative filepath for the document to create. It will be created relative to the library.
      FilePath ->
      -- \| Document content
      Text ->
      -- \| The absolute path of the created document
      IO FilePath
  }

setupSpecEnv :: SpecWith SpecEnv -> Spec
setupSpecEnv = around withTempLibrary

withTempLibrary :: (SpecEnv -> IO ()) -> IO ()
withTempLibrary action = do
  config <- mkSpecEnv
  initCommonTestDocuments config

  let logLibraryDirOnTestFailure = onException (action config) (logTestLibrary config)
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory config.libraryDir
    logLibraryDirOnTestFailure
  where
    logTestLibrary :: SpecEnv -> IO ()
    logTestLibrary config = printf "Used temp library dir: %s\n" config.libraryDir

initCommonTestDocuments :: SpecEnv -> IO ()
initCommonTestDocuments config = do
  config.createDoc
    config.testFiles.linkChain1.pathFromLibrary
    $ mkLink "to link 2" config.testFiles.linkChain2.pathFromLibrary
  config.createDoc
    config.testFiles.linkChain2.pathFromLibrary
    $ mkLink "to link 3" config.testFiles.linkChain3.pathFromLibrary
  config.createDoc
    config.testFiles.linkChain3.pathFromLibrary
    $ mkLink "to link 4" config.testFiles.linkChain4.pathFromLibrary
  config.createDoc config.testFiles.linkChain4.pathFromLibrary "This file just exists"
  config.createDoc config.testFiles.parent.pathFromLibrary "Parent"
  return ()

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
      { libraryDir = libDir,
        testFiles = Constants.testFiles' libDir,
        defaultArgs = args,
        dbPath = dbFile,
        createDoc = \path content -> putDoc libDir path [content]
      }

createTempLibraryDir :: IO FilePath
createTempLibraryDir = do
  systemTempDir <- getTemporaryDirectory
  rand <- randomRIO (1000000, 9999999) :: IO Int
  let randomLibDirName = "library-" ++ show rand
  let testLibPath = systemTempDir </> "md-graph-test" </> randomLibDirName
  createDirectoryIfMissing True testLibPath
  createDirectory $ testLibPath </> "subdir"
  return testLibPath

putDoc :: FilePath -> FilePath -> [Text] -> IO FilePath
putDoc libDir docName lines = do
  let docPath = libDir </> docName +<.> "md"
  withFile docPath WriteMode $ \handle ->
    Monad.mapM_ (Text.hPutStr handle) lines
  return docPath

putDoc' :: FilePath -> FilePath -> [Text] -> IO TestFile
putDoc' libDir docName lines = do
  let pathFromLibrary = docName +<.> "md"
  let docPath = libDir </> docName +<.> "md"
  withFile docPath WriteMode $ \handle ->
    Monad.mapM_ (Text.hPutStr handle) lines
  return $
    TestFile
      { absolute = docPath,
        pathFromLibrary = pathFromLibrary,
        pathFromCurrent = getPathFromCurrent docPath
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

-- | Annotate expectations by describing the expected case. The annotation will be printed in the test failure output. Intercept HUnitFailure exceptions and prepend a message to them. This is useful when you have multiple similar expectations.
step :: (HasCallStack) => String -> Expectation -> Expectation
step msg expectation = catch expectation catcher
  where
    msgWithFailed = msg ++ " FAILED: "
    catcher :: HUnitFailure -> Expectation
    catcher (HUnitFailure loc (Reason reasonMsg)) = Exception.throwIO $ HUnitFailure loc (Reason (msgWithFailed ++ reasonMsg))
    -- I think ExpectedButGot is only used with Test.HUnit.Lang.assertEqual, which I don't think hspec-expectations uses
    catcher (HUnitFailure loc (ExpectedButGot preface expected actual)) =
      let newPreface = Just $ maybe msgWithFailed (\p -> msgWithFailed ++ p) preface
       in Exception.throwIO . HUnitFailure loc $ ExpectedButGot newPreface expected actual
