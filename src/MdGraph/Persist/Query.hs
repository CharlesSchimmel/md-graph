{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-#  LANGUAGE TypeApplications  #-}
{-#  LANGUAGE RankNTypes  #-}

module MdGraph.Persist.Query where

import           MdGraph.Persist.Schema

import           Control.Monad                  ( mapM )
import           Control.Monad.Logger           ( NoLoggingT(..) )
import           Control.Monad.Reader           ( MonadIO(liftIO)
                                                , MonadReader(ask)
                                                , ReaderT
                                                , local
                                                )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text(..) )
import           Database.Esqueleto.Experimental
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

-- | Delete modified Documents (ie the TempDoc counterpart has a newer
-- Modified) so that they can be found when newDocs is run (we will have to
-- delete them anyway)
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

-- Files with no edges to or from
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

-- Files with no edges to
stranded = undefined
