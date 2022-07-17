{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-#  LANGUAGE TypeApplications  #-}

module MdGraph.Persist where

import           MdGraph.Persist.Schema

import           Control.Monad                  ( mapM )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text(..) )
import           Database.Esqueleto.Experimental
import           Database.Persist               ( Filter(..)
                                                , deleteWhere
                                                , insertMany
                                                )
import           Database.Persist.Sqlite        ( runSqlite )

insertEdges :: Text -> [Edge] -> IO [Key Edge]
insertEdges connString edges = runSqlite connString $ insertMany edges

insertTags :: Text -> [Tag] -> IO [Key Tag]
insertTags connString tags = runSqlite connString $ insertMany tags

insertDocuments :: Text -> [Document] -> IO (M.Map (Key Document) Document)
insertDocuments connString docs = runSqlite connString $ do
    keys <- insertMany docs
    getMany keys

insertTempDocuments :: Text -> [TempDocument] -> IO [Key TempDocument]
insertTempDocuments connString docs = runSqlite connString $ do
    deleteWhere ([] :: [Filter TempDocument])
    insertMany docs

-- | Find items in Temp not in Doc
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

-- | Find items in Temp not in Doc
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

-- | Delete Docs where the TempDoc counterpart has a newer Modified
-- | So that they can be find when newDocs is run (we will have to delete them
-- | anyway)
pruneModifiedDocs :: Text -> IO ()
pruneModifiedDocs connString = runSqlite connString $ delete $ do
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

-- copyNewDocs connString = runSqlite connString $ insertEntityMany
