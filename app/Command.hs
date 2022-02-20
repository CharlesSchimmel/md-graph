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
      | Backlinks SubgraphOptions
      deriving Show

data SubgraphOptions = SubgraphOptions
  { sgTargets    :: NonEmpty Node
  , sgInclNonex  :: Bool
  , sgInclStatic :: Bool
  , sgTagDir     :: TagDirection
  , sgDepth      :: Integer
  }
  deriving Show

