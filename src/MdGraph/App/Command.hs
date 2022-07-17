module MdGraph.App.Command where

import           Aux.HashSet
import           MdGraph
import           MdGraph.Graph
import           MdGraph.Node
import           MdGraph.TagDirection

import           Data.HashSet                  as S
import           Data.List.NonEmpty

data Command =
    Orphans
      | Unreachable
      | Nonexes
      | Statics
      | Subgraph SubgraphOptions
      | Backlinks BacklinkOptions
      deriving Show

data SubgraphOptions = SubgraphOptions
    { sgTargets    :: [Node]
    , sgInclNonex  :: Bool
    , sgInclStatic :: Bool
    , sgTagDir     :: TagDirection
    , sgDepth      :: Integer
    }
    deriving Show


data BacklinkOptions = BacklinkOptions
    { blTargets :: [Node]
    , blDepth   :: Integer
    }
    deriving Show

runCommand
    :: Command -> [(Node, Node)] -> HashSet FilePath -> IO (HashSet Node)

runCommand Orphans nodes allFiles = return $ orphans fwdMap bwdMap allFiles
    where (fwdMap, bwdMap) = buildMaps nodes

runCommand Unreachable nodes allFiles = return $ stranded fwdMap bwdMap
    where (fwdMap, bwdMap) = buildMaps nodes

runCommand Nonexes nodes allFiles = do
    let (fwdMap, bwdMap) = buildMaps nodes
    results <- weirdos $ Corpus fwdMap bwdMap allFiles
    return $ nonex results

runCommand Statics nodes allFiles = do
    let (fwdMap, bwdMap) = buildMaps nodes
    results <- weirdos $ Corpus fwdMap bwdMap allFiles
    return $ statix results

runCommand (Subgraph (SubgraphOptions targets incNonex incStatic tagDir depth)) nodes allFiles
    = do
        (Weirdos statix nonex) <- weirdos $ Corpus fwdGraph bwdGraph allFiles
        return . withNonex nonex . withStatic statix $ subgraph depth
                                                                fwdGraph
                                                                targets
  where
    (fwdGraph, bwdGraph) = buildMaps $ adjustTagDir tagDir nodes
    withNonex            = if incNonex then const id else flip S.difference
    withStatic           = if incStatic then const id else flip S.difference

runCommand (Backlinks (BacklinkOptions targets depth)) nodes allFiles =
    return $ subgraph depth bwdGraph targets
    where (fwdGraph, bwdGraph) = buildMaps nodes
