{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module MdGraph.File.Internal where

import Aux.Common
import Control.Applicative
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad as Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Hashable (Hashable)
import qualified Data.List as List
import Data.Maybe
import Data.Time (UTCTime)
import qualified Data.Traversable as Traversable
import GHC.Generics (Generic)
import MdGraph.File.Types
import MdGraph.Util
import System.Directory as D
import System.FilePath as F
import Prelude as P

class IsFile a where
  unFile :: a -> FilePath

data ExpandResult = ExpandResult
  { resultPath :: AbsolutePath,
    resultModTime :: UTCTime
  }
  deriving (Show, Eq, Ord)

maybeFile :: FilePath -> IO (Maybe FilePath)
maybeFile file = maybeTester D.doesFileExist file

maybeDirectory :: FilePath -> IO (Maybe FilePath)
maybeDirectory dir = maybeTester D.doesDirectoryExist dir

-- | Find documents in library and return them with FilePaths relative to the
-- library
findDocuments ::
  DefaultExtension -> [AbsolutePath] -> IO [File]
findDocuments defaultExt sourcePaths = do
  maybeFiles <- Traversable.mapM (traverseDir defaultExt . unAbsolutePath) sourcePaths
  let unmaybedFiles = catMaybes maybeFiles
  return $ Monad.join unmaybedFiles

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
traverseDir :: FileExtension -> FilePath -> IO (Maybe [File])
traverseDir extension basePath = do
  pathType <- getPathType basePath
  maybeExpandResults <- Traversable.sequence $ expand extension <$> pathType
  let fileResults = for maybeExpandResults $
        \expandResults -> for expandResults $
          \expandResult -> expandResultToFile basePath expandResult
  return fileResults

-- | Recursively explore _path_, and return files with _extension_
expand :: FilePath -> PathType -> IO [ExpandResult]
expand extension (F path) =
  if not . F.isExtensionOf extension $ path
    then return []
    else do
      modAt <- getModificationTime path
      return [ExpandResult (AbsolutePath path) modAt]
expand extension (D path) = do
  contents <- fmap (path </>) <$> D.listDirectory path
  contentTypes <- catMaybes <$> mapConcurrently getPathType contents
  join <$> mapConcurrently (expand extension) contentTypes

-- TODO: probably need to only accept a single library dir instead of multiple
-- or we could run into filepath collisions.

-- | canonicalize path and also convert tilde home directory reference to actual
trueAbsolutePathIO :: FilePath -> IO AbsolutePath
trueAbsolutePathIO path = do
  detilded <- detilde path
  absolute <- makeAbsolute detilded
  return $ AbsolutePath absolute

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

expandResultToFile :: FilePath -> ExpandResult -> File
expandResultToFile basePath ExpandResult {resultPath, resultModTime} =
  let absResultPath = unAbsolutePath resultPath
      relativePath = RelativePath $ makeRelative basePath absResultPath
   in File
        { absolutePath = resultPath,
          relativePath = relativePath,
          modificationTime = resultModTime
        }
