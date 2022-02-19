{-# LANGUAGE TupleSections #-}

module Lib
    ( corpus
    , Weirdos(..)
    , weirdos
    ) where

import           File
import           Graph
import           HashSet
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
import           Data.List                     as L
import           Data.Maybe
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Data.Traversable              as T
import           Data.Tuple
import           Debug.Trace
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F

trace' x = trace (show x) x

corpus :: FilePath -> TagDirection -> [FilePath] -> IO Corpus
corpus validExt tagDir paths = do
    files <- fmap normalise <$> deepFiles paths
    let applicableFiles = P.filter (F.isExtensionOf validExt) files
    (fwdMap, bwdMap) <- buildGraphs validExt tagDir applicableFiles
    return $ Corpus fwdMap bwdMap (S.fromList applicableFiles)

parseFile :: FilePath -> IO [Node]
parseFile file = do
    content <- sieveLinks <$> T.readFile file
    return $ fromMaybe [] content

-- should probably flatten this so that it Just returns [(FilePath, Node)]
-- and then build the graphs later
buildGraphs :: FilePath -> TagDirection -> [FilePath] -> IO (Graph, Graph)
buildGraphs defaultExtension tagDir files = do
    nodePairs <- parseNodes defaultExtension files
    return $ buildMaps tagDir nodePairs

parseNodes :: FilePath -> [FilePath] -> IO [(Node, Node)]
parseNodes defExt files = parseToTuples files >>= T.mapM fixNodes
  where
    fixNodes (source, node) = do
        fixed <- fixNode defExt source node
        return (Link source, fixed)

parseToTuples :: [FilePath] -> IO [(FilePath, Node)]
parseToTuples f = P.concat <$> T.mapM parseToTuple f
  where
    parseToTuple f = do
        parsedNodes <- parseFile f
        return $ fmap (f, ) parsedNodes

data Weirdos = Weirdos
    { statix :: HashSet FilePath
    , nonex  :: HashSet FilePath
    }
    deriving Show

weirdos :: Corpus -> IO Weirdos
weirdos (Corpus _ backward allFiles) = foldM fold
                                             (Weirdos S.empty S.empty)
                                             notInLibrary
  where
    linkTargets  = catMaybesS . S.map nodePath . M.keysSet $ backward
    notInLibrary = linkTargets `S.difference` allFiles
    fold w@Weirdos { statix, nonex } file = do
        xists <- D.doesPathExist file
        return $ if xists
            then w { statix = file `S.insert` statix }
            else w { nonex = file `S.insert` nonex }

fixNode :: FilePath -> FilePath -> Node -> IO Node
fixNode defExt source (  Link path) = Link <$> fixLink defExt source path
fixNode _      _      t@(Tag  _   ) = pure t
