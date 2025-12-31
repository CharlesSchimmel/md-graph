{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MdGraph.Persist where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import GHC.IO.Handle (hIsWritable)
import MdGraph.App.Arguments (DatabaseArg (DbFile))
import MdGraph.File.Internal (trueAbsolutePathIO)
import MdGraph.File.Types 
import qualified System.Directory as D
import System.IO
  ( IOMode
      ( ReadWriteMode,
        WriteMode
      ),
    withFile,
  )
import System.Posix.Temp

dbArgToConnString :: DatabaseArg -> IO (Maybe T.Text)
dbArgToConnString (DbFile ":memory:") = return . Just $ ":memory:"
dbArgToConnString (DbFile path) = do
  trueAbsPath@(AbsolutePath {unAbsolutePath}) <- trueAbsolutePathIO (T.unpack path)
  let absolutePathText = T.pack unAbsolutePath
  -- TODO:
  -- If it's a new file, then attempt creating an empty file
  -- Check if file can be Read-Written
  exists <- withFile unAbsolutePath ReadWriteMode hIsWritable
  return $ if exists then Just absolutePathText else Nothing
