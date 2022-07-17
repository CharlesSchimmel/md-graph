{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-#  LANGUAGE TypeApplications  #-}

module MdGraph.Persist where

import           MdGraph.Persist.Schema

import           Control.Monad                  ( mapM )
import           Data.Text                      ( Text(..) )
import           Database.Esqueleto.Experimental
import           Database.Persist               ( Filter(..)
                                                , deleteWhere
                                                , insertMany
                                                )
import           Database.Persist.Sqlite        ( runSqlite )

insertTempDocuments :: Text -> [TempDocument] -> IO [Key TempDocument]
insertTempDocuments connString docs = runSqlite connString $ do
    deleteWhere ([] :: [Filter TempDocument])
    insertMany docs

modifiedFiles :: Text -> IO [(Entity Document, Entity TempDocument)]
modifiedFiles connString = runSqlite connString $ select $ do
    (file :& tempFile) <-
        from $ table @Document `InnerJoin` table @TempDocument `on` do
            \(file :& tempFile) ->
                file ^. DocumentPath ==. tempFile ^. TempDocumentPath
    where_
        $  (file ^. DocumentModifiedAt)
        <. (tempFile ^. TempDocumentModifiedAt)
    pure (file, tempFile)

deletedFiles :: Text -> IO [Entity Document]
deletedFiles connString = runSqlite connString $ select $ do
    (file :& tempFiles) <-
        from
        $          table @Document
        `leftJoin` table @TempDocument
        `on`       \(file :& tempFiles) ->
                       just (file ^. DocumentPath) ==. tempFiles ?. TempDocumentPath
    where_ $ isNothing (tempFiles ?. TempDocumentPath)
    pure file

newFiles :: Text -> IO [Entity TempDocument]
newFiles connString = runSqlite connString $ select $ do
    (tempFile :& file) <-
        from
        $          table @TempDocument
        `leftJoin` table @Document
        `on`       \(tempFile :& file) ->
                       just (tempFile ^. TempDocumentPath) ==. file ?. DocumentPath
    where_ $ isNothing (file ?. DocumentPath)
    pure tempFile

-- | Must be called after pruneDeletedDocuments!
-- | Delete TempDocs that have not been modified
pruneUnchangedTempDocs connString = runSqlite connString $ delete $ do
    tempDoc <- from $ table @TempDocument
    where_ $ tempDoc ^. TempDocumentPath `in_` subSelectList
        (do
            (doc :& tempDoc) <-
                from $ table @Document `InnerJoin` table @TempDocument `on` do
                    \(doc :& tempDoc) ->
                        (doc ^. DocumentPath ==. tempDoc ^. TempDocumentPath)
                            &&. (   doc
                                ^.  DocumentModifiedAt
                                ==. tempDoc
                                ^.  TempDocumentModifiedAt
                                )
            pure $ doc ^. DocumentPath
        )

-- | Must be called before pruneUnchangedTempDocs!
-- | Delete Docmuments not found in most recent scan
-- | This should cascade to a document's Tags and Edges
pruneDeletedDocuments connString = runSqlite connString $ delete $ do
    file <- from $ table @Document
    where_ $ file ^. DocumentPath `notIn` subSelectList
        (do
            tf <- from $ table @TempDocument
            pure $ tf ^. TempDocumentPath
        )

