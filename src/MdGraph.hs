{-# LANGUAGE ScopedTypeVariables #-}
module MdGraph where

import           Aux.Map                       as M
import           MdGraph.App
import           MdGraph.App.Arguments
import           MdGraph.App.Logger
import           MdGraph.Config
import           MdGraph.File                   ( Files(..) )
import           MdGraph.Parse                  ( ParseResult(..)
                                                , Parses(..)
                                                )
import           MdGraph.Persist.Mapper        as Mapper
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
import           MdGraph.Node                   ( Link(..) )
import           MdGraph.Persist.Class          ( PreparesDb(..) )
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
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )

mdGraph :: Command -> App ()
mdGraph command = do
    prepareDatabase
    -- (flip $ maybe (pure ())) maybeCommand $ \command -> do
    logDebug . T.pack $ show command
    docPaths <- runCommand command
    liftIO $ F.mapM_ putStrLn docPaths

-- prepareDatabaseFile :: App ()
-- prepareDatabaseFile = do


prepareDatabase :: (Monad m, PreparesDb m, Logs m, Files m, Parses m) => m ()
prepareDatabase = do
    logDebug "Preparing database"
    migrate

    -- find documents
    logDebug "Finding documents"
    docs <- findDocuments
    let totalCt = P.length docs

    -- load all found documents into temp
    logDebug "Populating TempDocuments"
    insertTempDocuments $ Mapper.fromFile <$> docs

    logDebug "Pruning deleted Documents"
    deletedCt <- pruneDeletedDocuments

    logDebug "Pruning unchanged TempDocuments"
    unchangedCt <- pruneUnchangedTempDocuments

    logDebug "Pruning modified Documents"
    modifiedCt <- pruneModifiedDocuments

    logDebug "Finding new and modified TempDocuments"
    newTempDocs <- getNewDocuments
    let docsToInsert = fromTempDocument . entityVal <$> newTempDocs
        newCt        = P.length docsToInsert - fromIntegral modifiedCt

    reportDocumentCount totalCt     "total documents"
    reportDocumentCount unchangedCt "unchanged"
    reportDocumentCount deletedCt   "deleted"
    reportDocumentCount modifiedCt  "modified"
    reportDocumentCount newCt       "new"

    logDebug "Inserting new and modified Documents"
    newDocs <- insertDocuments docsToInsert

    let docKeyMap       = M.flop documentPath newDocs
        docPathsToParse = M.keys docKeyMap

    logDebug "Parsing new and modified Documents"
    parseResults   <- parseDocuments docPathsToParse
    updatedResults <- updateLinks parseResults

    let resultMap = M.fromList' file updatedResults
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

    logDebug "Inserting new edges"
    insertEdges newEdges

    logInfo
        . T.unwords
        $ ["Found", T.pack . show . P.length $ newTags, "new tags"]
    logDebug "Inserting new tags"
    insertTags newTags

    pure ()

reportDocumentCount num reason = do
    logInfo . T.unwords $ [T.pack . show $ num, reason]
    pure ()

-- | Update links to include the extension if they were defined without one,
-- and their link-path.md resolves to a real file
updateLinks
    :: forall m . (Monad m, Files m) => [ParseResult] -> m [ParseResult]
updateLinks results = do
    let tryResolveDoc result@ParseResult { links } = do
            resolvedLinks <- mapM tryResolveLink links
            return result { links = resolvedLinks }
        tryResolveLink :: (Monad m, Files m) => Link -> m Link
        tryResolveLink link@Link { linkPath } = do
            actualPath <- getQualifiedDocumentPath linkPath
            return $ link { linkPath = actualPath }

    P.mapM tryResolveDoc results
