{-# LANGUAGE TupleSections #-}

module Lib
    ( printSubgraph
    , getSubgraph
    ) where

import           File
import           Link
import           PParser

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( join )
import           Data.HashMap.Lazy              ( HashMap )
import qualified Data.HashMap.Lazy             as M

import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as S

import           Data.Maybe
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Debug.Trace
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F

-- A links to B with title and anchor

data Edge = Edge
    { source :: FilePath
    , link   :: Link
    }
    deriving Show

trace' x = trace (show x) x

printSubgraph rootFile = do
    foundFiles <- S.toList <$> getSubgraph rootFile
    P.mapM_ P.putStrLn foundFiles

readIntoNodes :: FilePath -> IO [FilePath]
readIntoNodes file = do
    exists <- (<|>) <$> maybeFile file <*> maybeFile (file ++ ".md")
    flip (maybe $ return []) exists $ \file' -> do
        content <- T.readFile file'
        let parseResult = P.map T.unpack <$> sieveLinks content

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
        let alreadyVisited = curFile `S.member` visitedFiles_
        if alreadyVisited
            then visitedFiles
            else S.union <$> visitedFiles <*> recurse curFile visitedFiles_

    nodesAndCurrent = pure $ S.insert file visitedNodes

parseFile :: FilePath -> IO [Text]
parseFile file = do
    content <- T.readFile file
    let parseResult = sieveLinks content
    return $ fromMaybe [] parseResult

deepYieldLinks :: [FilePath] -> IO Graph
deepYieldLinks files = do
    deepFiles <- join . catMaybes <$> P.mapM traverseDir files
    M.fromList <$> P.mapM parseIntoTuple deepFiles
  where
    parseIntoTuple x = (x, ) . P.map (fillExtension . T.unpack) <$> parseFile x

type Source = FilePath
type Sinks = [FilePath]
type Graph = HashMap Source Sinks


