module MdGraph.File
  ( Files (..),
  )
where

import Control.Applicative
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Hashable (Hashable)
import qualified Data.List
import Data.Maybe
import Data.Text as T
  ( pack,
    unwords,
  )
import Data.Time (UTCTime)
import Data.Traversable as T
import MdGraph.App (App (App))
import MdGraph.App.Logger (logDebug)
import MdGraph.Config
  ( Config (..),
    HasConfig (getConfig),
  )
import MdGraph.File.Internal (File (..))
import qualified MdGraph.File.Internal as Internal
import MdGraph.Util (trace')
import System.Directory as D
import System.FilePath as F
import Prelude as P

class Files m where
  -- | Detilde and ensure the given path is absolute. Does not check for file existence.
  trueAbsolutePath :: FilePath -> m FilePath

  -- | Check if a FilePath exists; Nothing if it doesn't, Just FilePath if it does.
  maybeFile :: FilePath -> m (Maybe FilePath)

  -- | Find all documents
  findDocuments :: m [File]

  -- | Figure out if a Destination path exists relative to a Source path
  relativizeWithExtension ::
    -- | The Source path
    FilePath ->
    -- | The Destination path
    FilePath ->
    m FilePath

  -- | Fix the document path if it resolves with an extension
  getQualifiedDocumentPath :: FilePath -> m FilePath

instance Files App where
  trueAbsolutePath = liftIO . Internal.trueAbsolutePathIO
  maybeFile = liftIO . Internal.maybeFile
  relativizeWithExtension source dest = do
    Config {defaultExtension} <- getConfig
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
