module Graph where

import           HashSet
import           Node
import           TagDirection

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( foldM
                                                , join
                                                )
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
import           Debug.Trace
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

subgraph :: Graph -> Node -> HashSet Node
subgraph graph = dfs graph S.empty

dfs :: Graph -> HashSet Node -> Node -> HashSet Node
dfs graph visited file = P.foldr fold (file `S.insert` visited) children
  where
    children = fromMaybe S.empty $ graph M.!? file
    fold cur visited = if cur `S.member` visited
        then visited
        else visited `S.union` dfs graph visited cur

orphans :: Graph -> Graph -> HashSet FilePath -> HashSet Node
orphans fwdGraph bwdGraph allFiles = fileNodes `S.difference` nonOrphans
  where
    nonOrphans = M.keysSet fwdGraph `S.union` M.keysSet bwdGraph
    fileNodes  = S.map Link allFiles

stranded :: Graph -> Graph -> HashSet Node
stranded fwdGraph bwdGraph =
    M.keysSet fwdGraph `S.difference` M.keysSet bwdGraph

buildMaps :: TagDirection -> [(Node, Node)] -> (Graph, Graph)
buildMaps tagDir fwdEdges = (fwdGraph, bwdGraph)
  where
    bwdGraph          = mapFromPairs $ P.map swap fwdLinks
    fwdGraph          = mapFromPairs $ links ++ tagsWithDirection
    fwdLinks          = links ++ tagsWithDirection
    tagsWithDirection = tags >>= tagSwap tagDir
    (tags, links)     = L.partition (isTag . snd) fwdEdges
    mapFromPairs      = M.fromListWith S.union . P.map (toSnd S.singleton)

toSnd :: (b -> c) -> (a, b) -> (a, c)
toSnd fn (a, b) = (a, fn b)
