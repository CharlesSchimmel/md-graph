module File
    ( reRelativize
    , traverseDir
    , maybeFile
    , fillExtension
    , resolveOrFill
    ) where

import           Control.Monad                  ( join )
import           Data.Maybe
import           Debug.Trace
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F

trace' x = trace (show x) x

doubleDot :: FilePath
doubleDot = ".."

-- unsafe, need to switch to safe tail
-- does not support oddly placed parent-traversal like `foo/bar/baz/../file-in-bar.md`
reRelativize :: FilePath -> FilePath -> FilePath
reRelativize source destination
    | not $ isRelative destination = destination
    | otherwise = joinDir $ parentedSourceDirs ++ unrelativeDest
  where
    sourceParts = splitDirectories $ takeDirectory source
    destParts   = splitDirectories destination
    pops        = P.foldr
        (\cur acc -> if cur == doubleDot then P.tail . acc else acc)
        id
        destParts
    parentedSourceDirs = pops sourceParts
    unrelativeDest     = P.filter (/= doubleDot) destParts

joinDir []    = ""
joinDir paths = P.foldr1 (</>) paths

maybeFile file = do
    exists <- D.doesFileExist file
    pure $ if exists then Just file else Nothing

data PathType = File FilePath | Dir FilePath deriving Show

-- assuming paths are either files or dirs is probably incorrect
getPathType :: FilePath -> IO (Maybe PathType)
getPathType path = do
    exists <- D.doesPathExist path
    isFile <- D.doesFileExist path
    if not exists
        then return Nothing
        else return . Just $ if isFile then File path else Dir path

traverseDir :: FilePath -> IO (Maybe [FilePath])
traverseDir path = do
    pathType <- getPathType path
    P.sequence $ expand' <$> pathType

expand' :: PathType -> IO [FilePath]
expand' (  File path) = return [path]
expand' p@(Dir  path) = do
    contents     <- fmap (path </>) <$> D.listDirectory path
    contentTypes <- catMaybes <$> P.mapM getPathType contents
    join <$> P.mapM expand' contentTypes

fillExtension path extension =
    if hasExtension path then path else path <.> extension

resolveOrFill :: FilePath -> FilePath -> IO FilePath
resolveOrFill file defaultExtension = do
    existsWithExt <- D.doesPathExist (file <.> defaultExtension)
    return $ if existsWithExt then file <.> defaultExtension else file

