{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module MdGraph
  ( mdGraph,
  )
where

import Aux.Map as M
import Aux.Tuple
  ( mapSnd,
    mapToSnd,
    mapToSndM,
  )
import Control.Concurrent.Async (mapConcurrently)
import qualified Control.Monad as Monad
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
  ( MonadReader (ask),
    asks,
  )
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Either (partitionEithers, rights)
import qualified Data.Either as Either
import Data.Foldable as F
  ( mapM_,
  )
import qualified Data.Foldable as Foldable
import Data.HashSet as S
  ( HashSet,
    fromList,
    member,
    toList,
  )
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist.Sqlite
  ( Entity (entityVal),
    runSqlite,
    wrapConnection,
  )
import Database.Sqlite (open)
import MdGraph.App
import MdGraph.App.Arguments
import MdGraph.App.Command (Command)
import MdGraph.App.Logger
import MdGraph.App.RunCommand (runCommand)
import MdGraph.Config
import MdGraph.File (Files (..), unrelativize)
import MdGraph.File.Types (AbsolutePath (..), File (..), RelativePath (..))
import MdGraph.Node (Link (..))
import MdGraph.Node as Node
import MdGraph.Parse
  ( ParseResult (..),
    Parses (..),
  )
import MdGraph.Persist.Class (PreparesDb (..))
import qualified MdGraph.Persist.Mapper as Mapper
import MdGraph.Persist.Schema
  ( Document (documentPath),
    Edge (..),
    Key (..),
    TempDocument (tempDocumentPath),
    migrateAll,
    migrateMdGraph,
  )
import qualified MdGraph.Persist.Schema as Schema
import MdGraph.Populate
import Options.Applicative
import System.FilePath
  ( makeRelative,
    (-<.>),
    (<.>),
    (</>),
  )
import Prelude as P

mdGraph :: Arguments -> IO (Either T.Text [String])
mdGraph args@Arguments {argCommand, argScan} = do
  conf <- runExceptT $ argsToConfig args
  -- TODO: Better error handling here
  Monad.join <$> mapM withConf conf
  where
    withConf conf = do
      conn <- open $ dbConnString conf
      sqlBackend <- wrapConnection conn (\_ _ _ _ -> return ())
      let env = Env conf sqlBackend
      -- It's a little pointless to have this function and mdGraph function.
      -- TODO: Merge them
      runExceptT (runReaderT (runApp $ prepareDbAndRun argCommand) env)
    prepareDbAndRun :: Command -> App [String]
    prepareDbAndRun command = do
      prepareDatabase
      populate argScan
      logDebug . T.pack $ show command
      runCommand command

prepareDatabase ::
  (Monad m, HasConfig m, PreparesDb m, Logs m, Files m, Parses m) => m ()
prepareDatabase = do
  Config {defaultExtension, libraryPath, dbConnString} <- getConfig
  logDebug $ T.unwords ["Using library:", T.pack $ unAbsolutePath libraryPath]
  logDebug $ T.unwords ["Preparing database:", dbConnString]
  migrate
  return ()
