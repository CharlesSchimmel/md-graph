{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-#  LANGUAGE TypeApplications  #-}
{-#  LANGUAGE RankNTypes  #-}

module MdGraph.Persist.Query
    ( insertEdges
    , insertTags
    , insertDocuments
    , insertTempDocuments
    , pruneDeletedDocuments
    , pruneUnchangedTempDocs
    , newFiles
    , unreachable
    , orphans
    , pruneModifiedDocs
    , nonexistant
    , forwardLinks
    , backwardLinks
    ) where

import           MdGraph.Persist.Schema

import           Control.Monad                  ( mapM )
import           Control.Monad.Logger           ( NoLoggingT(..) )
import           Control.Monad.Reader           ( MonadIO(liftIO)
                                                , MonadReader(ask)
                                                , ReaderT
                                                , local
                                                )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text(..) )
import           Data.Text                     as T
import           Database.Esqueleto.Experimental
import           Database.Esqueleto.Experimental.From.SqlSetOperation
                                                ( SqlSetOperation
                                                    ( unSqlSetOperation
                                                    )
                                                )
import           Database.Persist               ( Filter(..)
                                                , deleteWhere
                                                , insertMany
                                                )
import           Database.Persist.Sqlite        ( runSqlite )
import           UnliftIO.Resource              ( ResourceT(..) )

insertEdges :: MonadIO m => Text -> [Edge] -> m [Key Edge]
insertEdges connString edges = liftIO . runSqlite connString $ insertMany edges

insertTags :: MonadIO m => Text -> [Tag] -> m [Key Tag]
insertTags connString tags = liftIO . runSqlite connString $ insertMany tags

insertDocuments
    :: MonadIO m => Text -> [Document] -> m (M.Map (Key Document) Document)
insertDocuments connString docs = liftIO . runSqlite connString $ do
    keys <- insertMany docs
    getMany keys

insertTempDocuments
    :: MonadIO m => Text -> [TempDocument] -> m [Key TempDocument]
insertTempDocuments connString docs = liftIO . runSqlite connString $ do
    deleteWhere ([] :: [Filter TempDocument])
    insertMany docs

-- | Find items in Temp not in Doc
modifiedFiles
    :: MonadIO m => Text -> m [(Entity Document, Entity TempDocument)]
modifiedFiles connString = liftIO . runSqlite connString $ select $ do
    (file :& tempFile) <-
        from $ table @Document `InnerJoin` table @TempDocument `on` do
            \(file :& tempFile) ->
                file ^. DocumentPath ==. tempFile ^. TempDocumentPath
    where_
        $  (file ^. DocumentModifiedAt)
        <. (tempFile ^. TempDocumentModifiedAt)
    pure (file, tempFile)

-- | Find items in Temp not in Doc
newFiles :: MonadIO m => Text -> m [Entity TempDocument]
newFiles connString = liftIO . runSqlite connString $ select $ do
    (tempFile :& file) <-
        from
        $          table @TempDocument
        `leftJoin` table @Document
        `on`       \(tempFile :& file) ->
                       just (tempFile ^. TempDocumentPath) ==. file ?. DocumentPath
    where_ $ isNothing (file ?. DocumentPath)
    pure tempFile

-- | Must be called after pruneDeletedDocuments! Delete TempDocs that have not
-- been modified
pruneUnchangedTempDocs connString =
    liftIO . runSqlite connString $ deleteCount $ do
        tempDoc <- from $ table @TempDocument
        where_ $ tempDoc ^. TempDocumentPath `in_` subSelectList
            (do
                (doc :& tempDoc) <-
                    from
                    $           table @Document
                    `InnerJoin` table @TempDocument
                    `on`        do
                                    \(doc :& tempDoc) ->
                                        (   doc
                                            ^.  DocumentPath
                                            ==. tempDoc
                                            ^.  TempDocumentPath
                                            )
                                            &&. (   doc
                                                ^.  DocumentModifiedAt
                                                ==. tempDoc
                                                ^.  TempDocumentModifiedAt
                                                )
                pure $ doc ^. DocumentPath
            )

-- | Must be called before pruneUnchangedTempDocs!
-- Delete Docmuments not found in most recent scan This should cascade to a
-- document's Tags and Edges
pruneDeletedDocuments connString =
    liftIO . runSqlite connString . deleteCount $ do
        file <- from $ table @Document
        whereDocumentDeleted file

-- | Documents that are not in TempDocuments
whereDocumentDeleted file = do
    where_ $ file ^. DocumentPath `notIn` subSelectList
        (do
            tf <- from $ table @TempDocument
            pure $ tf ^. TempDocumentPath
        )

-- | Delete modified Documents (modified determined when the TempDoc
-- counterpart has a newer Modified) so that they can be found when newDocs is
-- run (we will have to delete them anyway)
pruneModifiedDocs connString = liftIO . runSqlite connString $ deleteCount $ do
    file <- from $ table @Document
    where_ $ file ^. DocumentPath `in_` subSelectList
        (do
            (doc :& tempDoc) <-
                from $ table @Document `InnerJoin` table @TempDocument `on` do
                    \(doc :& tempDoc) ->
                        (doc ^. DocumentPath ==. tempDoc ^. TempDocumentPath)
                            &&. (  doc
                                ^. DocumentModifiedAt
                                <. tempDoc
                                ^. TempDocumentModifiedAt
                                )
            pure $ doc ^. DocumentPath
        )

-- Files with no incoming or outgoing edges
orphans connString = liftIO . runSqlite connString $ select $ do
    from
        $         from (table @Document)
        `except_` filesThatHaveLinks
        `except_` filesThatAreLinkedTo

filesThatAreLinkedTo = do
    (file :& edge) <- from $ table @Document `InnerJoin` table @Edge `on` do
        \(doc :& edge) -> doc ^. DocumentPath ==. edge ^. EdgeHead
    pure file

filesThatHaveLinks = do
    (file :& edge) <- from $ table @Document `InnerJoin` table @Edge `on` do
        \(doc :& edge) -> doc ^. DocumentId ==. edge ^. EdgeTail
    pure file

-- Files with no incoming edges (but may have outgoing 
unreachable connString =
    liftIO
        . runSqlite connString
        $ select
        $ do
              from
        $ (filesThatHaveLinks `except_` filesThatAreLinkedTo)

backwardLinks :: MonadIO m => T.Text -> FilePath -> m [Entity Document]
backwardLinks connString docPath = liftIO . runSqlite connString $ select $ do
    (childDoc :& edge :& parentDoc) <-
        from
        $           table @Document
        `innerJoin` table @Edge
        `on`        (\(cd :& e) -> cd ^. DocumentPath ==. e ^. EdgeHead)
        `innerJoin` table @Document
        `on`        (\(_ :& e :& pd) -> pd ^. DocumentId ==. e ^. EdgeTail)
    where_ (childDoc ^. DocumentPath ==. val docPath)
    pure parentDoc

forwardLinks :: MonadIO m => T.Text -> FilePath -> m [Entity Document]
forwardLinks connString docPath = do
    documents <- liftIO . runSqlite connString $ select $ do
        (parentDoc :& edge :& childDoc) <-
            from
            $           table @Document
            `innerJoin` table @Edge
            `on` (\(doc :& edge) -> doc ^. DocumentId ==. edge ^. EdgeTail)
            `leftJoin`  table @Document
            `on`        (\(_ :& edge :& doc) ->
                            just (edge ^. EdgeHead) ==. doc ?. DocumentPath
                        )
        where_ (parentDoc ^. DocumentPath ==. val docPath)
        pure childDoc
    return $ catMaybes documents


-- | Edges without associated files
nonexistant connString = liftIO . runSqlite connString $ select $ do
    (edge :& doc) <-
        from $ table @Edge `leftJoin` table @Document `on` \(edge :& doc) ->
            just (edge ^. EdgeTail) ==. doc ?. DocumentId
    where_ $ isNothing (doc ?. DocumentPath)
    pure edge
