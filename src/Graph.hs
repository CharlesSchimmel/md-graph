module Graph where

import           HashSet
import           Node
import           TagDirection

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( foldM
                                                , join
                                                )
import           Data.Foldable                 as F
import           Data.HashMap.Lazy              ( HashMap )
import qualified Data.HashMap.Lazy             as M
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as S
import           Data.Hashable
import           Data.List                     as L
import           Data.Maybe
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Data.Tuple
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F

type Sinks = HashSet Node
type Graph = HashMap Node Sinks

data Corpus = Corpus
    { forward  :: Graph
    , backward :: Graph
    , allFiles :: HashSet FilePath
    }

tags :: Corpus -> HashSet Text
tags (Corpus fwd bwd _) =
    catMaybesS . S.map tagText $ flattenMap fwd `S.union` flattenMap bwd
  where
    flattenMap m = P.foldr S.union S.empty $ uncurry S.insert <$> M.toList m

subgraph :: Foldable f => Integer -> Graph -> f Node -> HashSet Node
subgraph maxDepth graph targets =
    S.fromList $ (F.toList . dfs maxDepth graph) =<< F.toList targets

dfs :: Integer -> Graph -> Node -> HashSet Node
dfs maxDepth graph file = go (0, maxDepth) graph S.empty file
  where
    go :: (Integer, Integer) -> Graph -> HashSet Node -> Node -> HashSet Node
    go (current, maxDepth) graph visited file = if current == maxDepth
        then visited
        else P.foldr fold (file `S.insert` visited) children
      where
        children = fromMaybe S.empty $ graph M.!? file
        fold cur visited = if cur `S.member` visited
            then visited
            else visited `S.union` go (current + 1, maxDepth) graph visited cur

orphans :: Graph -> Graph -> HashSet FilePath -> HashSet Node
orphans fwdGraph bwdGraph allFiles = fileNodes `S.difference` nonOrphans
  where
    nonOrphans = M.keysSet fwdGraph `S.union` M.keysSet bwdGraph
    fileNodes  = S.map Link allFiles

stranded :: Graph -> Graph -> HashSet Node
stranded fwdGraph bwdGraph =
    M.keysSet fwdGraph `S.difference` M.keysSet bwdGraph

buildMaps :: [(Node, Node)] -> (Graph, Graph)
buildMaps edges = (fwdGraph, bwdGraph)
  where
    bwdGraph     = mapFromPairs $ fmap swap edges
    fwdGraph     = mapFromPairs edges
    mapFromPairs = M.fromListWith S.union . fmap (toSnd S.singleton)

toSnd :: (b -> c) -> (a, b) -> (a, c)
toSnd fn (a, b) = (a, fn b)
