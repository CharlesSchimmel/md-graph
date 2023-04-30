module MdGraph.File.Internal where

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
import           Data.Time                      ( UTCTime )
import           Data.Traversable              as T
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F

data File = File
  { filePath         :: FilePath -- | Relative to the library
  , modificationTime :: UTCTime
  }
  deriving (Show, Ord, Eq)

doubleDot :: FilePath
doubleDot = ".."

-- unsafe, need to switch to safe tail
-- does not support oddly placed parent-traversal like `foo/bar/baz/../file-in-bar.md`
-- | Given a source path and a destination path, make the destination path
-- relative to the source
reRelativize :: FilePath -> FilePath -> FilePath
reRelativize source destination
  | not $ isRelative destination = destination
  | otherwise = joinDir $ parentedSourceDirs ++ unrelativeDest
 where
  sourceParts = splitDirectories $ takeDirectory source
  destParts = splitDirectories destination
  pops = P.foldr -- todo: use normalise?
                 (\cur acc -> if cur == doubleDot then P.tail . acc else acc)
                 id
                 destParts
  parentedSourceDirs = pops sourceParts
  unrelativeDest     = P.filter (/= doubleDot) destParts

joinDir []    = ""
joinDir paths = P.foldr1 (</>) paths

-- Figure out if a path exists relative to the file it came from. Check if a
-- path exists with extension, with reRelativization, with rerel and extension.
fixLink :: FilePath -> FilePath -> FilePath -> IO FilePath
fixLink defaultExtension source dest = fromMaybe dest <$> runMaybeT result
 where
  result =
    (normalise <$> tryExt defaultExtension dest)
      <|> (normalise <$> tryRerel source dest)
      <|> tryRerelExt defaultExtension source dest

tryExt :: FilePath -> FilePath -> MaybeT IO FilePath
tryExt defExt dest = MaybeT $ maybePath $ dest <.> defExt

tryRerel :: FilePath -> FilePath -> MaybeT IO FilePath
tryRerel source dest = MaybeT $ maybePath $ reRelativize source dest

tryRerelExt :: FilePath -> FilePath -> FilePath -> MaybeT IO FilePath
tryRerelExt defExt source dest =
  MaybeT $ maybePath $ reRelativize source (dest <.> defExt)

maybePath :: FilePath -> IO (Maybe FilePath)
maybePath path = do
  exists <- D.doesPathExist path
  return $ if exists then Just path else Nothing

maybeFile :: FilePath -> IO (Maybe FilePath)
maybeFile file = do
  exists <- D.doesFileExist file
  pure $ if exists then Just file else Nothing

maybeDirectory :: FilePath -> IO (Maybe FilePath)
maybeDirectory dir = do
  exists <- D.doesDirectoryExist dir
  pure $ if exists then Just dir else Nothing

-- | Find documents in library and return them with FilePaths relative to the
-- library
findDocuments
  :: (Traversable f, Foldable f) => FilePath -> f FilePath -> IO [File]
findDocuments defaultExt sourcePaths = do
  documents <- deepFiles defaultExt sourcePaths
  return $ normaliseDoc <$> documents
 where
  normaliseDoc doc@File { filePath } = doc { filePath = normalise filePath }

data PathType = F FilePath | D FilePath deriving Show

getPathType :: FilePath -> IO (Maybe PathType)
getPathType path = do
  exists <- D.doesPathExist path
  isFile <- D.doesFileExist path
  if not exists
    then return Nothing
    else return . Just $ if isFile then F path else D path

traverseDir :: FilePath -> FilePath -> IO (Maybe [File])
traverseDir extension path = do
  pathType <- getPathType path
  T.sequence $ expand extension <$> pathType

expand :: FilePath -> PathType -> IO [File]
expand extension path = go path
 where
  go :: PathType -> IO [File]
  go (F path) = if not . F.isExtensionOf extension $ path
    then return []
    else do
      modAt <- getModificationTime path
      return [File path modAt]
  go p@(D path) = do
    contents     <- fmap (path </>) <$> D.listDirectory path
    contentTypes <- catMaybes <$> mapConcurrently getPathType contents
    join <$> mapConcurrently go contentTypes

deepFiles :: (Traversable f, Foldable f) => FilePath -> f FilePath -> IO [File]
deepFiles extension sourcePath = join . catMaybes . toList <$> T.forM
  sourcePath
  (\file -> do
    result <- traverseDir extension file
    let relativized = fmap (relativizeDocument file) <$> result
    return relativized
  )

-- TODO: probably need to only accept a single library dir instead of multiple
-- or we could run into filepath collisions.

relativizeDocument :: FilePath -> File -> File
relativizeDocument baseDir doc@File { filePath } =
  doc { filePath = makeRelative baseDir filePath }

-- | canonicalize path and also convert tilde home directory reference to actual
trueAbsolutePathIO :: FilePath -> IO FilePath
trueAbsolutePathIO path = do
  detilde path >>= canonicalizePath

detilde :: FilePath -> IO FilePath
detilde path = do
  homePath <- getHomeDirectory
  let pathParts = splitPath path
  return $ rejoin homePath pathParts
 where
  rejoin homePath []                 = ""
  rejoin homePath ("~/" : pathParts) = joinPath (homePath : pathParts)
  rejoin _        pathParts          = joinPath pathParts
