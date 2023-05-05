{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module MdGraph.File.Internal where

import           Control.Applicative
import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Monad                  ( join )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader           ( asks )
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.Hashable                  ( Hashable )
import qualified Data.List                     as L
import           Data.Maybe
import           Data.Text                     as T
import           Data.Time                      ( UTCTime )
import           Data.Traversable              as T
import           GHC.Generics                   ( Generic )
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F

-- | Paths relative to the library directory
newtype RelativePath = RelativePath { unRelativePath :: FilePath }
  deriving (Show, Ord, Eq, Generic)

instance Hashable RelativePath

-- | Paths absolute to the filesystem
newtype AbsolutePath = AbsolutePath { unAbsolutePath :: FilePath }
  deriving (Show, Ord, Eq, Generic)

instance Hashable AbsolutePath

class IsFile a where
  unFile :: a -> FilePath

data FileResult = FileResult
  { resultPath    :: AbsolutePath
  , resultModTime :: UTCTime
  }
  deriving (Show, Eq, Ord)

data File = File
  { absolutePath     :: AbsolutePath
  , relativePath     :: RelativePath
  , modificationTime :: UTCTime
  }
  deriving (Show, Eq, Ord)

doubleDot :: FilePath
doubleDot = ".."

-- TODO? does not support oddly placed parent-traversal like `foo/bar/baz/../file-in-bar.md`
--
-- | If a destination path has parent directory traversal (../), flatten it
-- with its source to remove the directory traversal
reRelativize :: FilePath -> FilePath -> FilePath
reRelativize sourceFile destination
  | not $ isRelative destination = destination
  | otherwise                    = trueDest </> joinDir absoluteParts
 where
  sourceParts    = splitDirectories . takeDirectory $ sourceFile
  destParts      = splitDirectories destination
  isRelativePart = (== doubleDot)
  -- | Just the double dots
  relativeParts  = L.length . L.takeWhile isRelativePart $ destParts
  -- | The actually useful parts of the destination path that aren't double
  -- dots
  absoluteParts  = L.dropWhile isRelativePart destParts
  -- | The parts of the sourceFile path without the directories popped by the
  -- destination's ../'s
  trueDest =
    joinDir $ L.reverse . L.drop relativeParts . L.reverse $ sourceParts

-- TODO: why not joinPath?
joinDir []    = ""
joinDir paths = P.foldr1 (</>) paths

-- | Figure out if a path exists relative to the file it came from. Check if a
-- path exists with extension, with reRelativization, with rerel and extension.
fixLink :: FilePath -> FilePath -> FilePath -> IO FilePath
fixLink defaultExtension source dest = fromMaybe dest <$> runMaybeT result
 where
  result =
    (normalise <$> tryExt defaultExtension dest)
      <|> (normalise <$> tryRerel source dest)
      <|> tryRerelExt defaultExtension source dest

-- | Figure out if a path exists relative to the file it came from. Check if a
-- path exists with extension, with reRelativization, with rerel and extension.
smartRelativizePath
  :: Monad m
  => (FilePath -> m Bool)
  -> FilePath
  -> FilePath
  -> FilePath
  -> m FilePath
smartRelativizePath tester defaultExtension source dest = do
  let destWithExtension = dest -<.> defaultExtension
  destWithExtensionResult <- maybeTester tester destWithExtension
  rereled                 <- maybeTester tester $ reRelativize source dest
  rereledWithExtension    <- maybeTester tester
    $ reRelativize source destWithExtension
  return
    .   fromMaybe dest
    $   destWithExtensionResult
    <|> rereled
    <|> rereledWithExtension

maybeTester :: Monad m => (a -> m Bool) -> a -> m (Maybe a)
maybeTester tester a = do
  test <- tester a
  return $ if test then Just a else Nothing

tryExt :: FilePath -> FilePath -> MaybeT IO FilePath
tryExt defExt dest = MaybeT $ maybeFile $ dest <.> defExt

tryRerel :: FilePath -> FilePath -> MaybeT IO FilePath
tryRerel source dest = MaybeT $ maybeFile $ reRelativize source dest

tryRerelExt :: FilePath -> FilePath -> FilePath -> MaybeT IO FilePath
tryRerelExt defExt source dest =
  MaybeT $ maybeFile $ reRelativize source (dest <.> defExt)

maybeFile :: FilePath -> IO (Maybe FilePath)
maybeFile file = maybeTester D.doesFileExist file

maybeDirectory :: FilePath -> IO (Maybe FilePath)
maybeDirectory dir = maybeTester D.doesDirectoryExist dir

-- | Find documents in library and return them with FilePaths relative to the
-- library
findDocuments
  :: (Traversable f, Foldable f) => FilePath -> f FilePath -> IO [File]
findDocuments defaultExt sourcePaths = do
  join . catMaybes . toList <$> T.mapM (traverseDir defaultExt) sourcePaths

data PathType = F FilePath | D FilePath deriving Show

-- | Try to evaluate a path. If it exists, give its type.
getPathType :: FilePath -> IO (Maybe PathType)
getPathType path = do
  exists <- D.doesPathExist path
  isFile <- D.doesFileExist path
  if not exists
    then return Nothing
    else return . Just $ if isFile then F path else D path

-- | Try to get get the filetree of a path. If the path does not exist, return
-- Nothing.
traverseDir :: FilePath -> FilePath -> IO (Maybe [File])
traverseDir extension path = do
  pathType    <- getPathType path
  fileResults <- T.sequence $ expand extension <$> pathType
  return $ (fmap $ fmap (relativizeFile path)) fileResults

-- | Recursively explore _path_, and return files with _extension_
expand :: FilePath -> PathType -> IO [FileResult]
expand extension (F path) = if not . F.isExtensionOf extension $ path
  then return []
  else do
    modAt <- getModificationTime path
    return [FileResult (AbsolutePath path) modAt]
expand extension (D path) = do
  contents     <- fmap (path </>) <$> D.listDirectory path
  contentTypes <- catMaybes <$> mapConcurrently getPathType contents
  join <$> mapConcurrently (expand extension) contentTypes

-- TODO: probably need to only accept a single library dir instead of multiple
-- or we could run into filepath collisions.

-- | canonicalize path and also convert tilde home directory reference to actual
trueAbsolutePathIO :: FilePath -> IO AbsolutePath
trueAbsolutePathIO path = AbsolutePath <$> (detilde path >>= makeAbsolute)

makeRelative' :: AbsolutePath -> AbsolutePath -> RelativePath
makeRelative' (AbsolutePath a) (AbsolutePath b) =
  RelativePath $ makeRelative a b

detilde :: FilePath -> IO FilePath
detilde path = do
  homePath <- getHomeDirectory
  let pathParts = splitPath path
  return $ rejoin homePath pathParts
 where
  rejoin homePath []                 = ""
  rejoin homePath ("~/" : pathParts) = joinPath (homePath : pathParts)
  rejoin _        pathParts          = joinPath pathParts

relativizeFile :: FilePath -> FileResult -> File
relativizeFile basePath file@FileResult { resultPath, resultModTime } = File
  { absolutePath     = resultPath
  , relativePath     = makeRelativePath basePath resultPath
  , modificationTime = resultModTime
  }

makeRelativePath :: FilePath -> AbsolutePath -> RelativePath
makeRelativePath basePath (AbsolutePath aPath) =
  RelativePath $ makeRelative basePath aPath
