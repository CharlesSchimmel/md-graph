{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


module MdGraph.Persist.Schema where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

{- File:

   Tag:
   Composite key of name and file with uniqueness constraint

   Edge:
   Composite key of two files with uniqueness constraint
-}
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
File
    path String
    modifiedAt UTCTime

    UniquePath path
    deriving Show

TempFile
    path String
    modifiedAt UTCTime

    UniqueTempPath path
    deriving Show

Tag
    name String
    file FileId

    Primary name file
    UniqueTag name file
    deriving Show

Edge
    tail FileId
    head FileId

    Primary tail head
    UniqueEdge tail head
    deriving Show
|]

migrateMdGraph dbFile = runSqlite dbFile $ runMigration migrateAll
