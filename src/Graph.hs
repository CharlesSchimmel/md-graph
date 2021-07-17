module Graph where

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


subgraph :: Graph -> Node -> HashSet Node
subgraph graph = recurse graph S.empty

recurse :: Graph -> HashSet Node -> Node -> HashSet Node
recurse graph visited file = P.foldr fold (file `S.insert` visited) children
  where
    children = fromMaybe S.empty $ graph M.!? file
    fold cur visited = if cur `S.member` visited
        then visited
        else visited `S.union` recurse graph visited cur

orphans :: Graph -> Graph -> HashSet FilePath -> HashSet Node
orphans forGraph backGraph allFiles = fileNodes `S.difference` nonOrphans
  where
    nonOrphans = M.keysSet forGraph `S.union` M.keysSet backGraph
    fileNodes  = S.map Link allFiles

stranded :: Graph -> Graph -> HashSet Node
stranded forwGraph backGraph =
    M.keysSet forwGraph `S.difference` M.keysSet backGraph

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

