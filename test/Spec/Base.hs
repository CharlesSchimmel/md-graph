module Spec.Base where

import qualified Constants
import Control.Exception (evaluate)
import qualified Control.Exception as E
import Control.Monad (unless)
import qualified Control.Monad as Monad
import qualified Control.Monad as Traversable
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Either (fromRight, isRight)
import qualified Data.List as List
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
      argCommand = Command.Populate Command.PopulateAll
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
