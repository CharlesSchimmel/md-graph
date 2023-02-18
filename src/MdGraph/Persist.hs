{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-#  LANGUAGE TypeApplications  #-}
{-#  LANGUAGE RankNTypes  #-}

module MdGraph.Persist where

import           MdGraph.App.Arguments          ( DatabaseArg(DbFile, Temp) )

import qualified Data.Text                     as T
import qualified Data.Text                     as T
import qualified System.Directory              as D
import           System.Posix.Temp

dbArgToConnString :: DatabaseArg -> IO (Maybe T.Text)
dbArgToConnString Temp = do
    (path, handle) <- mkstemp "mdgraph"
    return . Just $ T.pack path

dbArgToConnString (DbFile path) = do
    exists <- D.doesFileExist $ T.unpack path
    return $ if exists then Just path else Nothing






