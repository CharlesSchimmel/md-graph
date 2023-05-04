{-# LANGUAGE StrictData  #-}
module MdGraph.Config where

import           Aux.Common                     ( explain )
import           Control.Monad.Except
import           Control.Monad.Logger           ( logDebug )
import qualified Data.Text                     as T
                                                ( Text(..)
                                                , pack
                                                , unwords
                                                )
import           Debug.Trace                    ( trace )
import           MdGraph.App.Arguments          ( Arguments(..) )
import           MdGraph.App.LogLevel
import           MdGraph.File.Internal          ( maybeDirectory
                                                , trueAbsolutePathIO
                                                )
import           MdGraph.Persist                ( dbArgToConnString )
import           System.Directory               ( canonicalizePath
                                                , doesDirectoryExist
                                                )
import           System.Posix                   ( directoryMode )

class HasConfig m where
  getConfig :: m Config

data Config = Config
  { logLevel         :: LogLevel
  , defaultExtension :: FilePath
  -- | Absolute library path
  , libraryPath      :: FilePath
  , dbConnString     :: T.Text
  }
  deriving Show

argsToConfig :: Arguments -> ExceptT T.Text IO Config
argsToConfig args@Arguments {..} = do
  Config argLogLevel argDefExt <$> libraryPath <*> dbPath
 where
  dbPath :: ExceptT T.Text IO T.Text
  dbPath = do
    mbDbPath <- liftIO $ dbArgToConnString argDatabase
    maybe (throwError "Invalid database path given") pure mbDbPath

  libraryPath :: ExceptT T.Text IO FilePath
  libraryPath = do
    absoluteLibraryDir <- liftIO $ trueAbsolutePathIO argLibrary
    liftIO (maybeDirectory absoluteLibraryDir)
      >>= maybe (throwError "Library directory does not exist") pure

