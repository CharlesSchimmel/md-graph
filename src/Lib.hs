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
type Sinks = [Sink]
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
    files <- deepFiles paths
    graph <- buildGraph files
    return $ Corpus graph (backGraph graph) (S.fromList files)

nodes :: [FilePath] -> IO [Node]
nodes paths = do
    files <- deepFiles paths
    graph <- buildGraph files
    return $ buildNodes graph (backGraph graph) files

subgraph :: Graph -> Source -> HashSet FilePath
subgraph graph = recurse graph S.empty

recurse :: Graph -> HashSet FilePath -> Source -> HashSet FilePath
recurse graph visited file = P.foldr fold (file `S.insert` visited) children
  where
    children = maybe S.empty S.fromList $ graph M.!? file
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
buildGraph :: [FilePath] -> IO Graph
buildGraph files = M.fromList . P.filter hasAny <$> P.mapM parseIntoTuple files
  where
    hasAny (_, xs) = not $ P.null xs
    parseIntoTuple x =
        (x, )
            .   P.map (fillExtension . reRelativize x . T.unpack)
            <$> parseFile x

backGraph :: Graph -> Graph
backGraph g = S.toList <$> P.foldr fold M.empty flattened
  where
    asList    = M.toList g
    flattened = asList >>= (\(source, sinks) -> P.map (, source) sinks)
    fold (sink, source) map =
        M.insertWith S.union sink (S.singleton source) map

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
                    (maybe [] (P.map go) $ fwds M.!? label)
                    (maybe [] (P.map go) $ bwds M.!? label)
