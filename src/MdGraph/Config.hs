module MdGraph.Config where

import           Aux.Common                     ( explain )
import           Control.Monad.Except
import qualified Data.Text                     as T
                                                ( Text(..)
                                                , pack
                                                , unwords
                                                )
import           MdGraph.App.Arguments          ( Arguments(..) )
import           MdGraph.App.LogLevel
import           MdGraph.Persist                ( dbArgToConnString )

data Config = Config
  { logLevel         :: LogLevel
  , defaultExtension :: FilePath
  , libraryPath      :: FilePath
  , dbConnString     :: T.Text
  }
  deriving Show

argsToConfig :: Arguments -> ExceptT T.Text IO Config
argsToConfig args@Arguments {..} = do
  mbDbPath <- liftIO $ dbArgToConnString argDatabase
  dbPath   <- maybe (throwError "Invalid database path given") pure mbDbPath
  return $ Config argLogLevel argDefExt argLibrary dbPath

