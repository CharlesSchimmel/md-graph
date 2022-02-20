module Main where

import           Arguments
import           Command
import           Graph
import           HashSet
import           Lib
import           Node

import           Control.Monad
import           Data.Foldable                 as F
import           Data.HashMap.Lazy             as M
import           Data.HashSet                  as S
import           Data.Maybe
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Options.Applicative
import           Prelude                       as P
import           System.Environment

main :: IO ()
main = do
    args    <- execParser opts
    files   <- retrieveFiles (argDefExt args) (argLibrary args)
    links   <- parseNodes (argDefExt args) files
    results <- doRun' (argCommand args) links $ S.fromList files
    F.mapM_ P.putStrLn . P.map show . S.toList $ results

doRun' :: Command -> [(Node, Node)] -> HashSet FilePath -> IO (HashSet Node)

doRun' Orphans nodes allFiles = return $ orphans fwdMap bwdMap allFiles
    where (fwdMap, bwdMap) = buildMaps nodes

doRun' Unreachable nodes allFiles = return $ stranded fwdMap bwdMap
    where (fwdMap, bwdMap) = buildMaps nodes

doRun' Nonexes nodes allFiles = do
    let (fwdMap, bwdMap) = buildMaps nodes
    results <- weirdos $ Corpus fwdMap bwdMap allFiles
    return . S.map Link $ nonex results

doRun' Statics nodes allFiles = do
    let (fwdMap, bwdMap) = buildMaps nodes
    results <- weirdos $ Corpus fwdMap bwdMap allFiles
    return . S.map Link $ statix results

doRun' (Subgraph (SubgraphOptions targets incNonex incStatic tagDir depth)) nodes allFiles
    = return $ subgraph depth fwdGraph targets
    where (fwdGraph, bwdGraph) = buildMaps $ adjustLinks tagDir nodes

doRun' (Backlinks (SubgraphOptions targets incNonex incStatic tagDir depth)) nodes allFiles
    = return $ subgraph depth bwdGraph targets
    where (fwdGraph, bwdGraph) = buildMaps $ adjustLinks tagDir nodes
