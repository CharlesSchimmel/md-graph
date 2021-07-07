module Lib
    ( someFunc
    ) where

import           File
import           Link
import           Parser

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( join )
import           Data.HashSet                  as S
import           Data.Maybe
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Debug.Trace
import           Prelude                       as P
import           System.Directory              as D

trace' x = trace (show x) x

someFunc rootFile = do
    foundFiles <- S.toList <$> getSubgraph rootFile
    P.mapM_ P.putStrLn foundFiles

maybeFile file = do
    exists <- D.doesFileExist file
    pure $ if exists then Just file else Nothing

readIntoNodes :: FilePath -> IO [FilePath]
readIntoNodes file = do
    exists <- (<|>) <$> maybeFile file <*> maybeFile (file ++ ".md")
    flip (maybe $ return []) exists $ \file' -> do
        content <- T.readFile file'
        let parseResult = P.map (T.unpack . uri) <$> parseLinks content

        extantChildren <- P.sequence $ P.mapM (pivotChild file) <$> parseResult
        let extant' = catMaybes <$> extantChildren

        return $ fromMaybe [] extant'

-- if the current file is "./foo/bar/baz.md"
-- and the child is referenced from the current file as "../child" then
-- we need to update the child to be "./foo/child"
pivotChild :: FilePath -> FilePath -> IO (Maybe FilePath)
pivotChild currentFile child = do
    let reRelativizedChild = reRelativize currentFile child
    (<|>) <$> maybeFile reRelativizedChild <*> maybeFile
        (reRelativizedChild ++ ".md")

getSubgraph file = recurse file S.empty

recurse :: FilePath -> HashSet FilePath -> IO (HashSet FilePath)
recurse file visitedNodes = do
    children <- readIntoNodes file
    P.foldr fold nodesAndCurrent children
  where
    children = readIntoNodes file
    fold :: FilePath -> IO (HashSet FilePath) -> IO (HashSet FilePath)
    fold curFile visitedFiles = do
        visitedFiles_ <- visitedFiles
        let alreadyVisited = curFile `member` visitedFiles_
        if alreadyVisited
            then visitedFiles
            else union <$> visitedFiles <*> recurse curFile visitedFiles_

    nodesAndCurrent = pure $ insert file visitedNodes
