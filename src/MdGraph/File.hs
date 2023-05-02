module MdGraph.File
    ( Files(..)
    ) where

import           Control.Applicative
import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Monad                  ( join )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader           ( asks )
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.Hashable                  ( Hashable )
import qualified Data.List
import           Data.Maybe
import           Data.Text                     as T
                                                ( pack
                                                , unwords
                                                )
import           Data.Time                      ( UTCTime )
import           Data.Traversable              as T
import           MdGraph.App                    ( App(App) )
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F

import           MdGraph.App.Logger             ( logDebug )
import           MdGraph.Config                 ( Config(..)
                                                , HasConfig(getConfig)
                                                )
import qualified MdGraph.File.Internal         as Internal
import           MdGraph.File.Internal          ( File(..) )
import           MdGraph.Util                   ( trace' )

class Files m where
  trueAbsolutePath :: FilePath -> m FilePath
  maybeFile :: FilePath -> m (Maybe FilePath)
  findDocuments :: m [File]
  relativizeWithExtension :: FilePath -> FilePath -> m FilePath
  -- | Fix the document path if it resolves with an extension
  getQualifiedDocumentPath :: FilePath -> m FilePath

instance Files App where
    trueAbsolutePath = liftIO . Internal.trueAbsolutePathIO
    maybeFile        = liftIO . Internal.maybeFile
    relativizeWithExtension source dest = do
        Config { defaultExtension } <- getConfig
        fixedLink <- liftIO $ Internal.fixLink defaultExtension source dest
        logDebug . T.pack . show $ fixedLink
        return fixedLink
    findDocuments = do
        config@Config {..} <- getConfig
        liftIO $ Internal.findDocuments defaultExtension [libraryPath]
    getQualifiedDocumentPath path = do
        Config {..} <- getConfig
        -- what about subdirs
        let withExtension = trace' $ path <.> defaultExtension
        maybeFullPathWithExtension <- maybeFile (libraryPath </> withExtension)
        return $ maybe path (const withExtension) maybeFullPathWithExtension
