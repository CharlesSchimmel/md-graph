module MdGraph.App.Command where

import           Aux.HashSet
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

data SubgraphTarget = FileTarget FilePath | TagTarget FilePath
    deriving Show

data SubgraphOptions = SubgraphOptions
    { sgTargets    :: [SubgraphTarget]
    , sgInclNonex  :: Bool
    , sgInclStatic :: Bool
    , sgTagDir     :: TagDirection
    , sgDepth      :: Integer
    }
    deriving Show


data BacklinkOptions = BacklinkOptions
    { blTargets :: [SubgraphTarget]
    , blDepth   :: Integer
    }
    deriving Show
