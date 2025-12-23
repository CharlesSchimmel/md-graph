{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}
module MdGraph.File
  ( Files (..),
    normaliseEvil,
    unrelativize,
    isAncestorOf,
  )
where

import Control.Monad.IO.Class (liftIO)
import MdGraph.App (App (App))
import MdGraph.Config
  ( Config (..),
    HasConfig (getConfig),
  )
import qualified MdGraph.File.Internal as Internal
import MdGraph.File.Types
import System.FilePath
import qualified System.FilePath as FilePath

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
  findDocuments = do
    config@Config {..} <- getConfig
    liftIO $ Internal.findDocuments defaultExtension [libraryPath]

-- TODO: Enforce source and dest as absolute _files_ (not dirs?)?
-- TODO: Detilde before reaching this function

-- | When an "source" file references a "dest" file, it may reference it
-- relative to itself. For example, the source file "\/foo\/bar\/baz.md" might
-- reference the destination "qux.md". We need the destination's path to become
-- "\/foo\/bar\/qux.md"
unrelativize :: AbsolutePath -> DestFilePath -> AbsolutePath
unrelativize (AbsolutePath source) dest
  | FilePath.isAbsolute dest = AbsolutePath dest
  | otherwise = normaliseEvil $ AbsolutePath unnormalisedDestDir
  where
    sourceDir = FilePath.takeDirectory source
    unnormalisedDestDir = sourceDir FilePath.</> dest

-- TODO: Just use canonicalizePath from System.Directory? That handles symlinks. It doesn't collapse the directory if it doesn't exist though, which doesn't work for testing.

-- | Normalise "./" and "../" in an absolute filepathh
-- This function is "evil" because it doesn't handle symlinks. In the real world /foo/../bar is not necessarily /bar.
normaliseEvil :: AbsolutePath -> AbsolutePath
normaliseEvil (AbsolutePath path) =
  let firstPassNormalisation = FilePath.normalise path -- System.FilePath.normalise handles more than just simplifying "./"
      pathParts = FilePath.splitDirectories firstPassNormalisation
      fullyNormalisedParts = _normalise [] pathParts
      rebuiltPath = foldl (</>) "/" fullyNormalisedParts
   in AbsolutePath rebuiltPath
  where
    _normalise :: [FilePath] -> [FilePath] -> [FilePath]
    _normalise (prev : acc) (".." : rem) = _normalise acc rem
    _normalise [] (".." : rem) = _normalise [] rem
    _normalise acc ("." : rem) = _normalise acc rem
    _normalise acc (cur : rem) = _normalise (cur : acc) rem
    _normalise acc [] = reverse acc

-- | Check if parentPath is in the tree of childPath, ex `\/foo` is in an ancestor of \/foo\/bar.md
isAncestorOf :: AbsolutePath -> AbsolutePath -> Bool
isAncestorOf (AbsolutePath parentPath) (AbsolutePath childPath) =
  let parentDirs = FilePath.splitDirectories parentPath
      childDirs = FilePath.splitDirectories childPath
      zipped = zip parentDirs childDirs
      commonDirs = takeWhile (\(parentDir, childDir) -> parentDir == childDir) zipped
   in length parentDirs == length commonDirs
