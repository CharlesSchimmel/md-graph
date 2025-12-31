module Spec.Base where

import qualified Constants
import Control.Exception.Base
import qualified Control.Monad as Monad
import qualified Control.Monad as Traversable
import Data.Either as Either
import qualified Data.List as List
import Data.Text as Text
import Data.Text.IO as Text
import MdGraph.App.Arguments
import qualified MdGraph.App.Command as Command
import qualified MdGraph.App.LogLevel as LogLevel
import qualified MdGraph.TagDirection as TagDirection
import System.Directory
import System.FilePath
import qualified System.IO
import Test.Hspec

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
      argCommand = Command.Populate Command.PopulateAll,
      argPopulate = PopAll
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
          -- System.Directory.removeFile tempDbPath
          return ()

  bracket
    acquire
    release
    run
