module Command where

import           Data.List.NonEmpty
import           Node
import           TagDirection

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
