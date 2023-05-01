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
    , unreachableM
    , orphansM
    , pruneModifiedDocs
    , nonexistant
    , forwardLinks
    , backwardLinks
    ) where

import           MdGraph.Persist.Schema

import           Aux.Common                     ( batch )
import           Control.Monad                  ( mapM )
import           Control.Monad.Logger           ( NoLoggingT(..) )
import           Control.Monad.Reader           ( MonadIO(liftIO)
                                                , MonadReader(ask)
                                                , ReaderT
                                                , asks
                                                , local
                                                )
import           Data.Int                       ( Int64 )
import qualified Data.List                     as List
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
import           Database.Persist.Sql           ( runSqlConn )
import           Database.Persist.Sqlite        ( runSqlite
                                                , withSqliteConn
                                                )
import           MdGraph.App                    ( App(App)
                                                , Env(config)
                                                )
import           MdGraph.Config                 ( Config(dbConnString) )
import           UnliftIO.Resource              ( ResourceT(..) )

insertEdges :: [Edge] -> Query [Key Edge]
insertEdges edges = insertMany edges

insertTags :: [Tag] -> Query [Key Tag]
insertTags tags = insertMany tags

insertDocuments :: [Document] -> Query (M.Map (Key Document) Document)
insertDocuments docs = do
    keys <- insertMany docs
    let keyBatches = batch 500 keys
    aoeu <- sequence (getMany <$> keyBatches)
    return $ List.foldr M.union M.empty aoeu

insertTempDocuments :: [TempDocument] -> Query [Key TempDocument]
insertTempDocuments docs = do
    deleteWhere ([] :: [Filter TempDocument])
    insertMany docs

-- | Find items in Temp not in Doc
modifiedFiles :: Query [(Entity Document, Entity TempDocument)]
modifiedFiles = select $ do
    (file :& tempFile) <-
        from $ table @Document `InnerJoin` table @TempDocument `on` do
            \(file :& tempFile) ->
                file ^. DocumentPath ==. tempFile ^. TempDocumentPath
    where_
        $  (file ^. DocumentModifiedAt)
        <. (tempFile ^. TempDocumentModifiedAt)
    pure (file, tempFile)

-- | Find items in Temp not in Doc
newFiles :: Query [Entity TempDocument]
newFiles = select $ do
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
pruneUnchangedTempDocs :: Query Int64
pruneUnchangedTempDocs = deleteCount $ do
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
-- Delete Docmuments not found in most recent scan This should cascade to a
-- document's Tags and Edges
pruneDeletedDocuments :: Query Int64
pruneDeletedDocuments = deleteCount $ do
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
pruneModifiedDocs :: Query Int64
pruneModifiedDocs = deleteCount $ do
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

-- | Files with no incoming or outgoing edges
orphansM :: Query [Entity Document]
orphansM = select $ do
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

-- | Files with no incoming edges (but may have outgoing 
unreachableM :: Query [Entity Document]
unreachableM =
    select
        $ do
              from
        $ (filesThatHaveLinks `except_` filesThatAreLinkedTo)

backwardLinks :: FilePath -> Query [Entity Document]
backwardLinks docPath = select $ do
    (childDoc :& edge :& parentDoc) <-
        from
        $           table @Document
        `innerJoin` table @Edge
        `on`        (\(cd :& e) -> cd ^. DocumentPath ==. e ^. EdgeHead)
        `innerJoin` table @Document
        `on`        (\(_ :& e :& pd) -> pd ^. DocumentId ==. e ^. EdgeTail)
    where_ (childDoc ^. DocumentPath ==. val docPath)
    pure parentDoc

forwardLinks :: FilePath -> Query [Entity Document]
forwardLinks docPath = do
    documents <- select $ do
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
nonexistant :: Query [Entity Edge]
nonexistant = select $ do
    (edge :& doc) <-
        from $ table @Edge `leftJoin` table @Document `on` \(edge :& doc) ->
            just (edge ^. EdgeTail) ==. doc ?. DocumentId
    where_ $ isNothing (doc ?. DocumentPath)
    pure edge
