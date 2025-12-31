{-# LANGUAGE StrictData #-}

module MdGraph.Config where

import Aux.Common (explain)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (logDebug)
import qualified Data.Text as T
  ( Text (..),
    pack,
    unwords,
  )
import Debug.Trace (trace)
import MdGraph.App.Arguments (Arguments (..))
import MdGraph.App.LogLevel
import MdGraph.File.Internal
  ( maybeDirectory,
    trueAbsolutePathIO,
  )
import MdGraph.File.Types (AbsolutePath (..))
import MdGraph.Persist (dbArgToConnString)
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
  )
import System.Posix (directoryMode)

class HasConfig m where
  getConfig :: m Config

data Config = Config
  { logLevel :: LogLevel,
    defaultExtension :: FilePath,
    -- | Absolute library path
    libraryPath :: AbsolutePath,
    dbConnString :: T.Text
  }
  deriving (Show)

argsToConfig :: Arguments -> ExceptT T.Text IO Config
argsToConfig args@Arguments {..} = do
  Config argLogLevel argDefExt <$> libraryPath <*> dbPath
  where
    dbPath :: ExceptT T.Text IO T.Text
    dbPath = do
      mbDbPath <- liftIO $ dbArgToConnString argDatabase
      maybe (throwError "Invalid database path given") pure mbDbPath

    libraryPath :: ExceptT T.Text IO AbsolutePath
    libraryPath = do
      absPath@AbsolutePath {unAbsolutePath = absoluteLibraryDir} <- liftIO $ trueAbsolutePathIO argLibrary
      directoryExists <- liftIO $ maybeDirectory absoluteLibraryDir
      maybe (throwError "Library directory does not exist") (const . return $ absPath) directoryExists
