{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module MdGraph.File.Internal where

import Control.Applicative
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.Maybe
import Data.Text as T
import Data.Time (UTCTime)
import Data.Traversable as T
import GHC.Generics (Generic)
import MdGraph.Util (trace', trace'')
import System.Directory as D
import System.FilePath as F
import Prelude as P

-- | Paths relative to the library directory
newtype RelativePath = RelativePath {unRelativePath :: FilePath}
  deriving (Show, Ord, Eq, Generic)

instance Hashable RelativePath

-- | Paths absolute to the filesystem
newtype AbsolutePath = AbsolutePath {unAbsolutePath :: FilePath}
  deriving (Show, Ord, Eq, Generic)

instance Hashable AbsolutePath

class IsFile a where
  unFile :: a -> FilePath

data FileResult = FileResult
  { resultPath :: AbsolutePath,
    resultModTime :: UTCTime
  }
  deriving (Show, Eq, Ord)

data File = File
  { absolutePath :: AbsolutePath,
    -- TODO: I don't know why this is part of this record. We can figure out the relative path after getting all of the files.
    relativePath :: RelativePath,
    modificationTime :: UTCTime
  }
  deriving (Show, Eq, Ord)

doubleDot :: FilePath
doubleDot = ".."

type DefaultExtension = FilePath

type FileExtension = FilePath

type SourceFilePath = FilePath

type DestFilePath = FilePath

-- TODO? does not support oddly placed parent-traversal like `foo/bar/baz/../file-in-bar.md`
-- On the other hand, neither does System.Directory's canonicalizePath. See https://neilmitchell.blogspot.com/2015/10/filepaths-are-subtle-symlinks-are-hard.html
-- We know that the destination must be accessible from the library root, so we could use findFileWith to get the library-relative path

-- | If a destination path has parent directory traversal (../), flatten it
-- with its source to remove the directory traversal
reRelativize :: SourceFilePath -> DestFilePath -> FilePath
reRelativize sourceFile destination
  | not $ isRelative destination = trace'' "notRelative" destination
  | otherwise = trace'' "isRelative" $ trueDest </> joinDir absoluteParts
  where
    sourceParts = splitDirectories . takeDirectory $ sourceFile
    destParts = splitDirectories destination
    isRelativePart = (== doubleDot)
    -- \| Just the double dots
    numRelativeParts = L.length . L.takeWhile isRelativePart $ destParts
    -- \| The actually useful parts of the destination path that aren't double
    -- dots
    absoluteParts = L.dropWhile isRelativePart destParts
    -- \| Take the source path parts (all of the directory names that comprise the
    -- full path); remove directories from the end equal to the number of '..'
    -- in the relative destination.
    trueDest =
      joinDir $ L.reverse . L.drop numRelativeParts . L.reverse $ sourceParts

-- TODO: why not joinPath?
joinDir [] = ""
joinDir paths = P.foldr1 (</>) paths

maybeTester :: (Monad m) => (a -> m Bool) -> a -> m (Maybe a)
maybeTester tester a = do
  test <- tester a
  return $ if test then Just a else Nothing

tryExt :: DefaultExtension -> FilePath -> MaybeT IO FilePath
tryExt defExt dest = MaybeT $ maybeFile $ dest <.> defExt

tryRerel :: FilePath -> FilePath -> MaybeT IO FilePath
tryRerel source dest = MaybeT $ do
  found <- maybeFile $ reRelativize source dest
  return $ trace'' "tryrerel" found

tryRerelExt :: DefaultExtension -> FilePath -> FilePath -> MaybeT IO FilePath
tryRerelExt defExt source dest =
  MaybeT $ maybeFile $ reRelativize source (dest <.> defExt)

maybeFile :: FilePath -> IO (Maybe FilePath)
maybeFile file = maybeTester D.doesFileExist file

maybeDirectory :: FilePath -> IO (Maybe FilePath)
maybeDirectory dir = maybeTester D.doesDirectoryExist dir

-- | Find documents in library and return them with FilePaths relative to the
-- library
findDocuments ::
  (Traversable f, Foldable f) => DefaultExtension -> f FilePath -> IO [File]
findDocuments defaultExt sourcePaths = do
  join . catMaybes . toList <$> T.mapM (traverseDir defaultExt) sourcePaths

data PathType = F FilePath | D FilePath deriving (Show)

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
  pathType <- getPathType path
  fileResults <- T.sequence $ expand extension <$> pathType
  return $ (fmap $ fmap (relativizeFile path)) fileResults

-- | Recursively explore _path_, and return files with _extension_
expand :: FilePath -> PathType -> IO [FileResult]
expand extension (F path) =
  if not . F.isExtensionOf extension $ path
    then return []
    else do
      modAt <- getModificationTime path
      return [FileResult (AbsolutePath path) modAt]
expand extension (D path) = do
  contents <- fmap (path </>) <$> D.listDirectory path
  contentTypes <- catMaybes <$> mapConcurrently getPathType contents
  join <$> mapConcurrently (expand extension) contentTypes

-- TODO: probably need to only accept a single library dir instead of multiple
-- or we could run into filepath collisions.

-- | canonicalize path and also convert tilde home directory reference to actual
trueAbsolutePathIO :: FilePath -> IO FilePath
trueAbsolutePathIO path = do
  detilde path >>= makeAbsolute

-- TODO: shouldn't the shell expand this before passing it in?
detilde :: FilePath -> IO FilePath
detilde path = do
  homePath <- getHomeDirectory
  let pathParts = splitPath path
  return $ rejoin homePath pathParts
  where
    rejoin homePath [] = ""
    rejoin homePath ("~/" : pathParts) = joinPath (homePath : pathParts)
    rejoin _ pathParts = joinPath pathParts

relativizeFile :: FilePath -> FileResult -> File
relativizeFile basePath file@FileResult {resultPath, resultModTime} =
  File
    { absolutePath = resultPath,
      relativePath = makeRelativePath basePath resultPath,
      modificationTime = resultModTime
    }

makeRelativePath :: FilePath -> AbsolutePath -> RelativePath
makeRelativePath basePath (AbsolutePath aPath) =
  RelativePath $ makeRelative basePath aPath
