module Lib
    ( someFunc
    ) where

import           Link
import           Parser

import           Control.Applicative            ( (<|>) )
import           Data.HashSet                  as S
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
        let parseResult = parseLinks content
        -- at this point we'll need to "pivot" child results if they're relative
        -- paths to the current file
        return $ maybe [] (P.map (T.unpack . uri)) parseResult

getSubgraph file = recurse file S.empty

recurse :: FilePath -> HashSet FilePath -> IO (HashSet FilePath)
recurse file visitedNodes = do
    children <- readIntoNodes file
    P.foldr fold nodesAndCurrent children
  where
    children :: IO [FilePath]
    children = readIntoNodes file
    fold :: FilePath -> IO (HashSet FilePath) -> IO (HashSet FilePath)
    fold curFile visitedFiles = do
        visitedFiles_ <- visitedFiles
        let alreadyVisited = curFile `member` visitedFiles_
        if alreadyVisited
            then visitedFiles
            else union <$> visitedFiles <*> recurse curFile visitedFiles_

    nodesAndCurrent = pure $ insert file visitedNodes
