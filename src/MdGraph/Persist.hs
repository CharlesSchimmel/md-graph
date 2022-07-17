{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-#  LANGUAGE TypeApplications  #-}

module MdGraph.Persist where

import           MdGraph.File                   ( Document(..) )
import           MdGraph.Persist.Schema

import           Control.Monad                  ( mapM )
import           Data.Text                      ( Text(..) )
import           Database.Esqueleto.Experimental
import           Database.Persist               ( Filter(..)
                                                , deleteWhere
                                                , insertMany
                                                )
import           Database.Persist.Sqlite        ( runSqlite )

insertTempDocuments connString docs = runSqlite connString $ do
    deleteWhere ([] :: [Filter TempFile])
    insertMany docs

modifiedFiles :: Text -> IO [(Entity File, Entity TempFile)]
modifiedFiles connString = runSqlite connString $ select $ do
    (file :& tempFile) <- from $ table @File `InnerJoin` table @TempFile `on` do
        \(file :& tempFile) -> file ^. FilePath ==. tempFile ^. TempFilePath
    where_ $ (file ^. FileModifiedAt) <. (tempFile ^. TempFileModifiedAt)
    pure (file, tempFile)

deletedFiles :: Text -> IO [Entity File]
deletedFiles connString = runSqlite connString $ select $ do
    (file :& tempFiles) <-
        from
        $          table @File
        `leftJoin` table @TempFile
        `on`       \(file :& tempFiles) ->
                       just (file ^. FilePath) ==. tempFiles ?. TempFilePath
    where_ $ isNothing (tempFiles ?. TempFilePath)
    pure file

newFiles :: Text -> IO [Entity TempFile]
newFiles connString = runSqlite connString $ select $ do
    (tempFile :& file) <-
        from
        $          table @TempFile
        `leftJoin` table @File
        `on`       \(tempFile :& file) ->
                       just (tempFile ^. TempFilePath) ==. file ?. FilePath
    where_ $ isNothing (file ?. FilePath)
    pure tempFile

-- delete from file where not exist in tempfile
pruneFiles connString filePaths = runSqlite connString $ delete $ do
    file <- from $ table @File
    where_ $ file ^. FilePath `notIn` subSelectList
        (do
            tf <- from $ table @TempFile
            pure $ tf ^. TempFilePath
        )

