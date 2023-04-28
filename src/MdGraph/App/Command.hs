{-# LANGUAGE StrictData #-}
module MdGraph.App.Command where

import           Aux.HashSet
import           MdGraph.TagDirection

import           Data.HashSet                  as S
import           Data.List.NonEmpty

data Command =
    -- | Just populate the db, don't return anything
    Populate
    -- | Find documents that don't have any forward or backward ldinks
      | Orphans
    -- | Find documents that don't have any backward links
      | Unreachable
    -- | Find links that don't resolve to actual documents
      | Nonexes
    -- | Find links that resolve to non-documents
      | Statics
    -- | Find the links of a document and its links' links, etc
      | Subgraph SubgraphOptions
    -- | Find the files that link to a document
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
