{-# LANGUAGE FlexibleInstances #-}

module MdGraph.App.Logger where

import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import qualified Data.Text                     as T
import           Data.Text.IO                  as Tio
import           Prelude                 hiding ( log )

import           Control.Monad.Reader           ( asks )
import           Debug.Trace                    ( trace )
import           MdGraph.App                    ( App(..)
                                                , Env(..)
                                                )
import           MdGraph.App.LogLevel
import           MdGraph.Config
import           System.IO                      ( stderr )

class Logs m where
  log :: LogLevel -> T.Text -> m ()

instance Logs IO where
    log logLevel msg = Tio.hPutStrLn stderr $ formatMsg logLevel msg
      where
        formatMsg :: LogLevel -> T.Text -> T.Text
        formatMsg ll msg =
            T.concat ["[", T.toUpper . T.pack . show $ ll, "]: ", msg]

logError :: Logs m => T.Text -> m ()
logError = log Error

logDebug :: Logs m => T.Text -> m ()
logDebug = log Debug

logInfo :: Logs m => T.Text -> m ()
logInfo = log Info

instance Logs App where
    log msgLogLevel msg = do
        minLogLevel <- asks $ logLevel . config
        if minLogLevel <= msgLogLevel
            then liftIO $ log msgLogLevel msg
            else pure ()
