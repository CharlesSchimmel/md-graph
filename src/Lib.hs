{-# LANGUAGE TupleSections #-}

module Lib
    ( printSubgraph
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

type Source = FilePath
type Sink = FilePath
type Sinks = HashSet Sink
type Graph = HashMap Source Sinks

data Corpus = Corpus
    { forward  :: Graph
    , backward :: Graph
    , allFiles :: HashSet FilePath
    }

trace' x = trace (show x) x

printSubgraph rootFile = do
    undefined

corpus :: [FilePath] -> IO Corpus
corpus paths = do
    files              <- deepFiles paths
    (graph, backGraph) <- buildGraphs files
    return $ Corpus graph backGraph (S.fromList files)

nodes :: [FilePath] -> IO [Node]
nodes paths = do
    files              <- deepFiles paths
    (graph, backGraph) <- buildGraphs files
    return $ buildNodes graph backGraph files

subgraph :: Graph -> Source -> HashSet FilePath
subgraph graph = recurse graph S.empty

recurse :: Graph -> HashSet FilePath -> Source -> HashSet FilePath
recurse graph visited file = P.foldr fold (file `S.insert` visited) children
  where
    children = fromMaybe S.empty $ graph M.!? file
    fold cur visited = if cur `S.member` visited
        then visited
        else visited `S.union` recurse graph visited cur

parseFile :: FilePath -> IO [Text]
parseFile file = do
    content <- T.readFile file
    let parseResult = sieveLinks content
    return $ fromMaybe [] parseResult

deepFiles :: [FilePath] -> IO [FilePath]
deepFiles files = join . catMaybes <$> P.mapM traverseDir files

-- should try to build forward and backward graphs at once
buildGraphs :: [FilePath] -> IO (Graph, Graph)
buildGraphs files = do
    parsed <- P.mapM parseIntoTuple files
    return $ P.foldr fold (M.empty, M.empty) parsed
  where
    fold (f, b) (fs, bs) = (fs `union_squared` f, bs `union_squared` b)
    union_squared = M.unionWith S.union
    parseIntoTuple x = do
        parseResult <- P.map (fillExtension . reRelativize x . T.unpack)
            <$> parseFile x
        let
            forward = M.filter (not . S.null)
                $ M.fromList [(x, S.fromList parseResult)]
        let backward = M.fromList $ P.map (, S.singleton x) parseResult
        return (forward, backward)

orphans :: Graph -> Graph -> HashSet FilePath -> HashSet FilePath
orphans forGraph backGraph allFiles = allFiles `S.difference` nonOrphans
    where nonOrphans = M.keysSet forGraph `S.union` M.keysSet backGraph

stranded :: Graph -> Graph -> HashSet FilePath
stranded forwGraph backGraph =
    M.keysSet forwGraph `S.difference` M.keysSet backGraph

data Node = Node
    { label :: String
    , fwd   :: [Node]
    , bwd   :: [Node]
    }

instance Show Node where
    show = label

buildNodes :: Graph -> Graph -> [FilePath] -> [Node]
buildNodes fwds bwds all = P.map go all
  where
    go label = Node label
                    (maybe [] (P.map go . S.toList) $ fwds M.!? label)
                    (maybe [] (P.map go . S.toList) $ bwds M.!? label)
