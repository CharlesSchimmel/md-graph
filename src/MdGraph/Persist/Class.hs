module MdGraph.Persist.Class where

import Control.Monad.Reader
  ( MonadIO (liftIO),
    asks,
  )
import Data.Int (Int64)
import Data.Map.Strict as M
import Data.Text (Text)
import Database.Persist (Entity)
import Database.Persist.Sqlite (runSqlPersistM, runSqlite)
import MdGraph.App
  ( App (App),
    Env (config, sqliteBackend),
  )
import MdGraph.Config (Config (dbConnString))
import qualified MdGraph.Persist.Query as Q
import MdGraph.Persist.Schema
  ( Document,
    Edge,
    Key,
    Query,
    Tag,
    TempDocument,
    migrateMdGraph,
  )

class RunsQuery m where
  runQuery :: Query a -> m a

instance RunsQuery App where
  runQuery query = do
    conn <- asks sqliteBackend
    liftIO . flip runSqlPersistM conn $ query

class PreparesDb m where
  migrate :: m [Text]
  insertEdges :: [Edge] -> m [Key Edge]
  insertTags :: [Tag] -> m [Key Tag]
  insertDocuments :: [Document] -> m (M.Map (Key Document) Document)
  insertTempDocuments :: [TempDocument] -> m [Key TempDocument]
  getNewDocuments :: m [Entity TempDocument]

  -- | Must be called after pruneDeletedDocuments! Delete TempDocs that have not been modified
  pruneUnchangedTempDocuments :: m Int64

  -- | Delete Documents that do not exist in TempDocuments
  pruneDeletedDocuments :: m Int64

  -- | Delete modified Documents (modified determined when the TempDoc
  -- counterpart has a newer Modified) so that they can be found when newDocs is
  -- run (we will have to delete them anyway)
  pruneModifiedDocuments :: m Int64

  deleteDocuments :: [FilePath] -> m Int64

instance PreparesDb App where
  migrate = runQuery migrateMdGraph
  insertEdges = runQuery . Q.insertEdges
  insertTags = runQuery . Q.insertTags
  insertDocuments = runQuery . Q.insertDocuments
  insertTempDocuments = runQuery . Q.insertTempDocuments
  getNewDocuments = runQuery Q.newFiles
  pruneUnchangedTempDocuments = runQuery Q.pruneUnchangedTempDocs
  pruneDeletedDocuments = runQuery Q.pruneDeletedDocuments
  pruneModifiedDocuments = runQuery Q.pruneModifiedDocs
  deleteDocuments = runQuery . Q.deleteDocuments

class Queries m where
  getOrphans :: m [Entity Document]
  getUnreachables :: m [Entity Document]
  getForwardLinks :: FilePath -> m [Either (Entity Edge) (Entity Document)]
  getBackwardLinks :: FilePath -> m [Entity Document]
  getNonexistents :: m [Entity Edge]

instance Queries App where
  getOrphans = runQuery Q.orphansM
  getUnreachables = runQuery Q.unreachableM
  getForwardLinks = runQuery . Q.forwardLinks
  getBackwardLinks = runQuery . Q.backwardLinks
  getNonexistents = runQuery Q.nonexistent
