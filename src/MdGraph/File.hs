module MdGraph.File
    ( reRelativize
    , traverseDir
    , fixLink
    , maybeFile
    , deepFiles
    , retrieveFiles
    ) where

import           Control.Applicative
import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Monad                  ( join )
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.Hashable                  ( Hashable )
import           Data.Maybe
import           Data.Traversable              as T
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F

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

data PathType = File FilePath | Dir FilePath deriving Show

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
    T.sequence $ expand <$> pathType

expand :: PathType -> IO [FilePath]
expand (  File path) = return [path]
expand p@(Dir  path) = do
    contents     <- fmap (path </>) <$> D.listDirectory path
    contentTypes <- catMaybes <$> mapConcurrently getPathType contents
    join <$> mapConcurrently expand contentTypes

deepFiles :: (Traversable f, Foldable f) => f FilePath -> IO [FilePath]
deepFiles files = join . catMaybes . toList <$> T.mapM traverseDir files

retrieveFiles
    :: (Traversable f, Foldable f) => FilePath -> f FilePath -> IO [FilePath]
retrieveFiles defaultExt paths = do
    files <- fmap normalise <$> deepFiles paths
    return $ P.filter (F.isExtensionOf defaultExt) files

