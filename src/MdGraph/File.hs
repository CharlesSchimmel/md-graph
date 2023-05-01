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

import           MdGraph.Config                 ( Config(..)
                                                , HasConfig(getConfig)
                                                )
import qualified MdGraph.File.Internal         as Internal
import           MdGraph.File.Internal          ( File(..) )

class Files m where
  trueAbsolutePath :: FilePath -> m FilePath
  maybePath :: FilePath -> m (Maybe FilePath)
  maybeFile :: FilePath -> m (Maybe FilePath)
  findDocuments :: m [File]
  -- | Fix the document path if it resolves with an extension
  getQualifiedDocumentPath :: FilePath -> m FilePath

instance Files App where
    trueAbsolutePath = liftIO . Internal.trueAbsolutePathIO
    maybePath        = liftIO . Internal.maybePath
    maybeFile        = liftIO . Internal.maybeFile
    findDocuments    = do
        config@Config {..} <- getConfig
        liftIO $ Internal.findDocuments defaultExtension [libraryPath]
    getQualifiedDocumentPath path = do
        Config {..} <- getConfig
        let withExtension = path <.> defaultExtension
        maybeFullPathWithExtension <- maybeFile (libraryPath </> withExtension)
        return $ maybe path (const withExtension) maybeFullPathWithExtension
