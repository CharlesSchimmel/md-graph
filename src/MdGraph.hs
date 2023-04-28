module MdGraph where

import           Aux.Map                       as M
import           MdGraph.App
import           MdGraph.App.Arguments
import           MdGraph.App.Logger
import           MdGraph.Config
import           MdGraph.File                   ( FindsDocuments(findDocumentsM)
                                                , findDocuments
                                                )
import           MdGraph.Parse                  ( ParseResult(..)
                                                , parseDocument
                                                )
import           MdGraph.Persist.Mapper        as Mapper
import           MdGraph.Persist.Query
import           MdGraph.Persist.Schema         ( Document(documentPath)
                                                , Edge(..)
                                                , Tag(..)
                                                , TempDocument(tempDocumentPath)
                                                , migrateAll
                                                , migrateMdGraph
                                                )

import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Monad                  ( forM )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( MonadReader(ask)
                                                , asks
                                                )
import           Data.Foldable                 as F
                                                ( mapM_ )
import           Data.HashSet                  as S
                                                ( fromList
                                                , toList
                                                )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Database.Persist.Sqlite        ( Entity(entityVal)
                                                , runSqlite
                                                )
import           Debug.Trace                    ( trace )
import           MdGraph.App.Command            ( Command )
import           MdGraph.App.RunCommand         ( runCommand )
import           Options.Applicative
import           Prelude
import           Prelude                       as P
                                                ( foldr
                                                , length
                                                , map
                                                , mapM
                                                , print
                                                , putStrLn
                                                )
import           System.FilePath                ( (</>) )

mdGraph :: Command -> App ()
mdGraph command = do
    prepareDatabase
    logDebug . T.pack $ show command
    docPaths <- runCommand command
    liftIO $ F.mapM_ putStrLn docPaths

-- prepareDatabaseFile :: App ()
-- prepareDatabaseFile = do


prepareDatabase :: App ()
prepareDatabase = do
    Config {..} <- asks config
    logDebug "Preparing database"
    migrateMdGraph dbConnString

    -- find documents
    logDebug "Finding documents"
    docs <- liftIO $ findDocuments defaultExtension [libraryPath]
    let totalCt = P.length docs

    -- load all found documents into temp
    logDebug "Populating TempDocuments"
    insertTempDocuments dbConnString $ Mapper.fromFile <$> docs

    logDebug "Pruning deleted Documents"
    deletedCt <- pruneDeletedDocuments dbConnString

    logDebug "Pruning unchanged TempDocuments"
    unchangedCt <- pruneUnchangedTempDocs dbConnString

    logDebug "Pruning modified Documents"
    modifiedCt <- pruneModifiedDocs dbConnString

    logDebug "Finding new and modified TempDocuments"
    newTempDocs <- newFiles dbConnString
    let docsToInsert = fromTempDocument . entityVal <$> newTempDocs
        newCt        = P.length docsToInsert - fromIntegral modifiedCt

    reportDocumentCount totalCt     "total documents"
    reportDocumentCount unchangedCt "unchanged"
    reportDocumentCount deletedCt   "deleted"
    reportDocumentCount modifiedCt  "modified"
    reportDocumentCount newCt       "new"

    logDebug "Inserting new and modified Documents"
    newDocs <- insertDocuments dbConnString docsToInsert

    let docKeyMap       = M.flop documentPath newDocs
        docPathsToParse = M.keys docKeyMap

    logDebug "Parsing new and modified Documents"
    -- TODO: Need to fix link targets, missing extensions
    parseResults <-
        liftIO
        $   catMaybes
        <$> mapConcurrently (parseDocument defaultExtension libraryPath)
                            docPathsToParse

    -- TODO: Why are parse results not working?
    let resultMap = M.fromList' file parseResults
        pathToKeyResult =
            P.map snd . M.toList $ M.unionWith' docKeyMap resultMap
        newTagsAndEdges :: [([Tag], [Edge])]
        newTagsAndEdges = P.map
            (\(docKey, result) ->
                (fromParseResult docKey result, fromParseResult docKey result)
            )
            pathToKeyResult
        (newTags, newEdges) = P.foldr
            (\(accTags, accEdges) (tags, edges) ->
                (accTags ++ tags, accEdges ++ edges)
            )
            ([], [])
            newTagsAndEdges

    logInfo
        . T.unwords
        $ ["Found", T.pack . show . P.length $ newEdges, "new edges"]

    -- P.print newEdges
    logDebug "Inserting new edges"
    insertEdges dbConnString newEdges

    logInfo
        . T.unwords
        $ ["Found", T.pack . show . P.length $ newEdges, "new tags"]
    logDebug "Inserting new tags"
    insertTags dbConnString newTags

    pure ()

reportDocumentCount num reason = do
    logInfo . T.unwords $ [T.pack . show $ num, reason]
    pure ()
