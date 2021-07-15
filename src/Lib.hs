{-# LANGUAGE TupleSections #-}

module Lib where

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

corpus :: FilePath -> [FilePath] -> IO Corpus
corpus validExt paths = do
    files <- deepFiles paths
    let applicableFiles = P.filter (F.isExtensionOf validExt) files
    (fwdMap, bwdMap) <- buildGraphs applicableFiles validExt
    return $ Corpus fwdMap bwdMap (S.fromList applicableFiles)

nodes :: FilePath -> [FilePath] -> IO (HashMap FilePath Node)
nodes validExt paths = do
    files              <- deepFiles paths
    (graph, backGraph) <- buildGraphs files validExt
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
    let parseResult = fmap ignoreAnchors <$> sieveLinks content
    return $ fromMaybe [] parseResult

ignoreAnchors :: Text -> Text
ignoreAnchors = T.takeWhile (/= '#')

deepFiles :: [FilePath] -> IO [FilePath]
deepFiles files = join . catMaybes <$> P.mapM traverseDir files

buildGraphs :: [FilePath] -> FilePath -> IO (Graph, Graph)
buildGraphs files defaultExtension = do
    parsed <- P.mapM parseIntoTuple files
    return $ P.foldr fold (M.empty, M.empty) parsed
  where
    fold (f, b) (fs, bs) = (fs `union_squared` f, bs `union_squared` b)
    union_squared = M.unionWith S.union
    parseIntoTuple x = do
        parsedLinks   <- P.map (reRelativize x . T.unpack) <$> parseFile x
        resolvedLinks <- P.mapM (resolveOrFill defaultExtension) parsedLinks
        let
            forward = M.filter (not . S.null)
                $ M.fromList [(x, S.fromList parsedLinks)]
        let backward = M.fromList $ P.map (, S.singleton x) parsedLinks
        return (forward, backward)

orphans :: Graph -> Graph -> HashSet FilePath -> HashSet FilePath
orphans forGraph backGraph allFiles = allFiles `S.difference` nonOrphans
    where nonOrphans = M.keysSet forGraph `S.union` M.keysSet backGraph

stranded :: Graph -> Graph -> HashSet FilePath
stranded forwGraph backGraph =
    M.keysSet forwGraph `S.difference` M.keysSet backGraph

data Node = Node
    { label :: String
    , fwd   :: HashMap String Node
    , bwd   :: HashMap String Node
    }

instance Show Node where
    show = show . label

buildNodes :: Graph -> Graph -> [FilePath] -> HashMap FilePath Node
buildNodes fwds bwds all = M.fromList $ P.map build all
  where
    fromGraph g l =
        maybe M.empty (M.fromList . P.map build . S.toList) $ g M.!? l
    build label =
        (label, Node label (fromGraph fwds label) (fromGraph bwds label))
