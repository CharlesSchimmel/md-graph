{-# LANGUAGE TupleSections #-}

module Lib
    ( corpus
    , Weirdos(..)
    , weirdos
    , retrieveFiles
    , parseNodes
    , adjustLinks
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
import           Data.List.NonEmpty            as NE
import           Data.Maybe
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Data.Traversable              as T
import           Data.Tuple
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F

corpus :: FilePath -> TagDirection -> NonEmpty FilePath -> IO Corpus
corpus validExt tagDir paths = do
    files     <- retrieveFiles validExt paths
    nodePairs <- parseNodes validExt files
    let tagDirectionAdjustedNodePairs = nodePairs >>= tagSwap tagDir
        (fwdMap, bwdMap)              = buildMaps tagDirectionAdjustedNodePairs
    return $ Corpus fwdMap bwdMap (S.fromList files)

adjustLinks :: TagDirection -> [(Node, Node)] -> [(Node, Node)]
adjustLinks tagDir links = links >>= tagSwap tagDir

retrieveFiles
    :: (Traversable f, Foldable f) => FilePath -> f FilePath -> IO [FilePath]
retrieveFiles defaultExt paths = do
    files <- fmap normalise <$> deepFiles paths
    return $ P.filter (F.isExtensionOf defaultExt) files

parseFile :: FilePath -> IO [Node]
parseFile file = do
    content <- sieveLinks <$> T.readFile file
    return $ fromMaybe [] content

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
    { statix :: HashSet Node
    , nonex  :: HashSet Node
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
            then w { statix = Link file `S.insert` statix }
            else w { nonex = Link file `S.insert` nonex }

fixNode :: FilePath -> FilePath -> Node -> IO Node
fixNode defExt source (  Link path) = Link <$> fixLink defExt source path
fixNode _      _      t@(Tag  _   ) = pure t
