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
import           Control.Monad.Logger           ( NoLoggingT )
import           Control.Monad.Reader           ( ReaderT )
import           Control.Monad.Trans.Resource   ( ResourceT )
import           Data.Text                      ( Text )
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Quasi
import           Database.Persist.Sqlite
import           Database.Persist.TH

{- File:

   Tag:
   Composite key of name and file with uniqueness constraint

   Edge: Composite key of two files with uniqueness constraint. Trying to use
   "proper" graph terms: "tail" is the source file, "head" is what it's
   referencing
-}
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Document
    path String
    Primary path

    modifiedAt UTCTime

    deriving Show

TempDocument
    path String
    Primary path

    modifiedAt UTCTime

    deriving Show

Tag
    name String
    file DocumentId OnDeleteCascade OnUpdateCascade

    deriving Show

Edge
    tail DocumentId OnDeleteCascade OnUpdateCascade
    head String
    label String
    Primary tail head
    UniqueEdge tail head

    deriving Show
|]

type Query a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a

migrateMdGraph :: Query [Text]
migrateMdGraph = runMigrationQuiet migrateAll
