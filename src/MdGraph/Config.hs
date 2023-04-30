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
import           MdGraph.File                   ( maybeDirectory
                                                , trueAbsolutePath
                                                )
import           MdGraph.Persist                ( dbArgToConnString )
import           System.Directory               ( canonicalizePath
                                                , doesDirectoryExist
                                                )
import           System.Posix                   ( directoryMode )

data Config = Config
  { logLevel         :: LogLevel
  , defaultExtension :: FilePath
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
    absoluteLibraryDir <- liftIO $ trueAbsolutePath argLibrary
    liftIO (maybeDirectory absoluteLibraryDir)
      >>= maybe (throwError "Library directory does not exist") pure

