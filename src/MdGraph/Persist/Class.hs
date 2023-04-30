module MdGraph.Persist.Class where

import           Control.Monad.Reader           ( MonadIO(liftIO)
                                                , asks
                                                )
import           Data.Int                       ( Int64 )
import           Data.Map.Strict               as M
import           Data.Text                      ( Text )
import           Database.Persist               ( Entity )
import           Database.Persist.Sqlite        ( runSqlite )
import           MdGraph.App                    ( App(App)
                                                , Env(config)
                                                )
import           MdGraph.Config                 ( Config(dbConnString) )
import qualified MdGraph.Persist.Query         as Q
import           MdGraph.Persist.Schema         ( Document
                                                , Edge
                                                , Key
                                                , Query
                                                , Tag
                                                , TempDocument
                                                , migrateMdGraph
                                                )

class RunsQuery m where
  runQuery :: Query a -> m a

instance RunsQuery App where
    runQuery query = do
        conn <- asks $ dbConnString . config
        liftIO . runSqlite conn $ query

class PreparesDb m where
  migrate :: m [Text]
  insertEdges :: [Edge] -> m [Key  Edge]
  insertTags :: [Tag] -> m [Key Tag]
  insertDocuments :: [Document] -> m (M.Map (Key Document) Document)
  insertTempDocuments :: [TempDocument] -> m [Key TempDocument]
  getNewDocuments :: m [Entity TempDocument]
  pruneUnchangedTempDocuments :: m Int64
  pruneDeletedDocuments :: m Int64
  pruneModifiedDocuments :: m Int64

instance PreparesDb App where
    migrate                     = runQuery migrateMdGraph
    insertEdges                 = runQuery . Q.insertEdges
    insertTags                  = runQuery . Q.insertTags
    insertDocuments             = runQuery . Q.insertDocuments
    insertTempDocuments         = runQuery . Q.insertTempDocuments
    getNewDocuments             = runQuery Q.newFiles
    pruneUnchangedTempDocuments = runQuery Q.pruneUnchangedTempDocs
    pruneDeletedDocuments       = runQuery Q.pruneDeletedDocuments
    pruneModifiedDocuments      = runQuery Q.pruneModifiedDocs


class Queries m where
  getOrphans :: m [Entity Document]
  getUnreachables :: m [Entity Document]
  getForwardLinks :: FilePath -> m [Entity Document]
  getBackwardLinks :: FilePath -> m [Entity Document]
  getNonexistants :: m [Entity Edge]

instance Queries App where
    getOrphans       = runQuery Q.orphansM
    getUnreachables  = runQuery Q.unreachableM
    getForwardLinks  = runQuery . Q.forwardLinks
    getBackwardLinks = runQuery . Q.backwardLinks
    getNonexistants  = runQuery Q.nonexistant
