
module MdGraph.App where

import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                )
import           Control.Monad.IO.Class
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , asks
                                                )
import           Data.Text

newtype App a = App
  { runApp :: ReaderT Env (ExceptT Text IO) a
  } deriving (Monad, Functor, Applicative, MonadReader Env, MonadIO, MonadError Text)

data Env = Env
    { config :: Config
    }

data Config = Config
