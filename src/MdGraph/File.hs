{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}
module MdGraph.File
  ( Files (..),
    Internal.AbsolutePath (..),
    Internal.RelativePath (..),
    normaliseEvil,
    unrelativize,
    isAncestorOf,
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

instance Files App where
  trueAbsolutePath = liftIO . Internal.trueAbsolutePathIO
  maybeFile = liftIO . Internal.maybeFile

  -- Never used
  -- relativizeWithExtension source dest = do
  --   Config {defaultExtension} <- getConfig
  --   fixedLink <- liftIO $ Internal.fixLink defaultExtension source dest
  --   logDebug . T.pack . show $ fixedLink
  --   return fixedLink
  findDocuments = do
    config@Config {..} <- getConfig
    liftIO $ Internal.findDocuments defaultExtension [libraryPath]

type RebasedFilePath = FilePath

-- -- | When an "source" file references a "dest" file, it may reference it
-- -- relative to itself. For example, the source file "/foo/bar/baz.md" might
-- -- reference the destination "qux.md". We need the destination's path to become
-- -- "/foo/bar/qux.md"
-- TODO: Enforce source and dest as absolute _files_ (not dirs?)?
-- TODO: Detilde before reaching this function
unrelativize :: Internal.AbsolutePath -> Internal.DestFilePath -> IO Internal.AbsolutePath
unrelativize (Internal.AbsolutePath source) dest
  | isAbsolute dest = return $ Internal.AbsolutePath dest
  | otherwise = do
      let sourceDir = takeDirectory source
          unnormalisedDestDir = sourceDir </> dest
      return . normaliseEvil $ Internal.AbsolutePath unnormalisedDestDir

-- | Normalise "./" and "../" in an absolute filepathh
-- This function is "evil" because it doesn't handle symlinks. In the real world /foo/../bar is not necessarily /bar.
-- TODO: Just use canonicalizePath from System.Directory? That handles symlinks. It doesn't collapse the directory if it doesn't exist though, which doesn't work for testing.
normaliseEvil :: Internal.AbsolutePath -> Internal.AbsolutePath
-- normaliseEvil (Internal.AbsolutePath path) = Internal.AbsolutePath . foldl (</>) "" $ _normalise [] parts
normaliseEvil (Internal.AbsolutePath path) = Internal.AbsolutePath . foldl (</>) "/" $ _normalise [] parts
  where
    -- normaliseEvil (Internal.AbsolutePath path) = Internal.AbsolutePath . show $ _normalise [] parts

    parts = splitDirectories path
    _normalise :: [FilePath] -> [FilePath] -> [FilePath]
    _normalise (prev : acc) (".." : rem) = _normalise acc rem
    _normalise [] (".." : rem) = _normalise [] rem
    _normalise acc ("." : rem) = _normalise acc rem
    _normalise acc (cur : rem) = _normalise (cur : acc) rem
    _normalise acc [] = reverse acc

-- | Check if parentPath is in the tree of childPath, ex /foo is in an ancestor of /foo/bar.md
isAncestorOf :: Internal.AbsolutePath -> Internal.AbsolutePath -> Bool
isAncestorOf (Internal.AbsolutePath parentPath) (Internal.AbsolutePath childPath) = length parentDirs == length commonDirs
  where
    parentDirs = splitDirectories parentPath
    childDirs = splitDirectories childPath
    zipped = zip parentDirs childDirs
    commonDirs = takeWhile (\(parentDir, childDir) -> parentDir == childDir) zipped
