module MdGraph
    ( corpus
    , Weirdos(..)
    , weirdos
    ) where

import           Aux.HashSet
import           MdGraph.File
import           MdGraph.Graph
import           MdGraph.Node
import           MdGraph.Parse
import           MdGraph.TagDirection          as TagDir

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
