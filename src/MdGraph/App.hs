{-# LANGUAGE StrictData #-}
{-# LANGUAGE RankNTypes #-}

module MdGraph.App where

import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                )
import           Control.Monad.IO.Class
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , asks
                                                )
import qualified Data.Text                     as T
                                                ( Text(..)
                                                , pack
                                                , unwords
                                                )
import           Data.Text.IO                  as Tio
import           Database.Persist.SqlBackend    ( SqlBackend )

import           Control.Monad.Trans.Resource   ( MonadUnliftIO(..) )
import           MdGraph.App.LogLevel
import           MdGraph.Persist


newtype App a = App
  { runApp :: ReaderT Env (ExceptT T.Text IO) a
  } deriving (Monad, Functor, Applicative, MonadReader Env, MonadIO, MonadError T.Text)

data Env = Env
  { config :: Config
  }

data Config = Config
  { logLevel         :: LogLevel
  , defaultExtension :: FilePath
  , libraryPath      :: FilePath
  , dbConnString     :: T.Text
  }
  deriving Show
