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
dbArgToConnString (DbFile ":memory:") = pure . pure $ ":memory:"
dbArgToConnString (DbFile path) = do
  absPath <- T.pack <$> trueAbsolutePathIO (T.unpack path)
  -- TODO:
  -- If it's a new file, then attempt creating an empty file
  -- Check if file can be Read-Written
  exists <- liftIO $ withFile (T.unpack absPath) ReadWriteMode hIsWritable
  return $ if exists then Just absPath else Nothing
