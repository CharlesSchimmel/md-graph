{-# LANGUAGE TupleSections #-}

module Lib where

import           File
import           Node
import           PParser
import           TagDirection                  as TagDir

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( foldM
                                                , join
                                                )
import           Data.HashMap.Lazy              ( HashMap )
import qualified Data.HashMap.Lazy             as M

import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as S
import           Data.Hashable

import           Data.Maybe
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Debug.Trace
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F

type Sinks = HashSet Node
type Graph = HashMap Node Sinks

trace' x = trace (show x) x

data Corpus = Corpus
    { forward  :: Graph
    , backward :: Graph
    , allFiles :: HashSet FilePath
    }

corpus :: FilePath -> TagDirection -> [FilePath] -> IO Corpus
corpus validExt tagDir paths = do
    files <- fmap normalise <$> deepFiles paths
    let applicableFiles = P.filter (F.isExtensionOf validExt) files
    (fwdMap, bwdMap) <- buildGraphs validExt tagDir applicableFiles
    return $ Corpus fwdMap bwdMap (S.fromList applicableFiles)

subgraph :: Graph -> Node -> HashSet Node
subgraph graph = recurse graph S.empty

recurse :: Graph -> HashSet Node -> Node -> HashSet Node
recurse graph visited file = P.foldr fold (file `S.insert` visited) children
  where
    children = fromMaybe S.empty $ graph M.!? file
    fold cur visited = if cur `S.member` visited
        then visited
        else visited `S.union` recurse graph visited cur

parseFile :: FilePath -> IO [Node]
parseFile file = do
    content <- sieveLinks <$> T.readFile file
    return $ fromMaybe [] content

deepFiles :: [FilePath] -> IO [FilePath]
deepFiles files = join . catMaybes <$> P.mapM traverseDir files

-- should probably flatten this so that it Just returns [(FilePath, Node)]
-- and then build the graphs later
buildGraphs :: FilePath -> TagDirection -> [FilePath] -> IO (Graph, Graph)
buildGraphs defaultExtension tagDir files = do
    parsed <- P.mapM parseIntoTuple files
    return $ P.foldr fold (M.empty, M.empty) parsed
  where
    fold (f, b) (fs, bs) = (fs `union_squared` f, bs `union_squared` b)
    union_squared = M.unionWith S.union
    parseIntoTuple source = do
        parsedNodes <- parseFile source
        fixedNodes  <- P.mapM (fixNode defaultExtension source) parsedNodes
        let sourceNode = Link source
            tags       = P.filter isTag fixedNodes
            links      = P.filter isLink fixedNodes
            sinks      = S.fromList links
            sources    = links ++ inTags tagDir tags
            sourceTags =
                M.fromList . P.map (, S.singleton sourceNode) $ outTags
                    tagDir
                    tags
        let forward =
                M.filter (not . S.null) $ M.fromList [(sourceNode, sinks)]
        let backward = M.fromList $ P.map (, S.singleton sourceNode) sources
        return (forward `M.union` sourceTags, backward)

-- for Out, we need to include a forward entry of Tag -> Source
-- for In, we need to include a backward entry of Source -> Tag
outTags :: TagDirection -> [Node] -> [Node]
outTags In _    = []
outTags _  tags = tags

inTags :: TagDirection -> [Node] -> [Node]
inTags Out _    = []
inTags _   tags = tags

orphans :: Graph -> Graph -> HashSet FilePath -> HashSet Node
orphans forGraph backGraph allFiles = fileNodes `S.difference` nonOrphans
  where
    nonOrphans = M.keysSet forGraph `S.union` M.keysSet backGraph
    fileNodes  = S.map Link allFiles

stranded :: Graph -> Graph -> HashSet Node
stranded forwGraph backGraph =
    M.keysSet forwGraph `S.difference` M.keysSet backGraph

data Weirdos = Weirdos
    { statix :: HashSet FilePath
    , nonex  :: HashSet FilePath
    }
    deriving Show

weirdos :: Corpus -> IO Weirdos
weirdos corp = foldM fold (Weirdos S.empty S.empty) notInLibrary
  where
    linkTargets  = catMaybesS . S.map nodePath . M.keysSet $ backward corp
    notInLibrary = linkTargets `S.difference` allFiles corp
    fold w@Weirdos { statix, nonex } file = do
        xists <- D.doesPathExist file
        return $ if xists
            then w { statix = file `S.insert` statix }
            else w { nonex = file `S.insert` nonex }

catMaybesS :: (Eq a, Hashable a) => HashSet (Maybe a) -> HashSet a
catMaybesS = S.foldr fold S.empty
  where
    fold (Just a) acc = a `S.insert` acc
    fold Nothing  acc = acc

fixNode :: FilePath -> FilePath -> Node -> IO Node
fixNode defExt source (  Link path) = Link <$> fixLink defExt source path
fixNode _      _      t@(Tag  _   ) = pure t
