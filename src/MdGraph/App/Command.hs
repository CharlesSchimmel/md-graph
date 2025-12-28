{-# LANGUAGE StrictData #-}

module MdGraph.App.Command where

import Aux.HashSet
import Data.HashSet as S
import Data.List.NonEmpty
import MdGraph.TagDirection
import MdGraph.File.Types

data Command
  = -- | Just populate the db, don't return anything
    Populate PopulateOptions
  | -- | Find documents that don't have any forward or backward ldinks
    Orphans
  | -- | Find documents that don't have any backward links
    Unreachable
  | -- | Find links that don't resolve to actual documents
    Nonexes
  | -- | Find links that resolve to non-documents
    Statics
  | -- | Find the links of a document and its links' links, etc
    Subgraph SubgraphOptions
  | -- | Find the files that link to a document
    Backlinks BacklinkOptions
  deriving (Show)

data PopulateOptions = PopulateAll | PopulateTargets { popTargets :: [FilePath] }
  deriving Show

data SubgraphTarget = FileTarget FilePath | TagTarget FilePath
  deriving (Show)

data SubgraphOptions = SubgraphOptions
  { sgTargets :: [SubgraphTarget], -- TODO: Subgraph of a tag does not really make sense, tags can only be linked to
    sgInclNonex :: Bool,
    sgInclStatic :: Bool,
    sgTagDir :: TagDirection,
    sgMaxDepth :: Integer,
    sgMinDepth :: Integer
  }
  deriving (Show)

subgraphDefaultMaxDepth = -1

-- By default, include the target file.
subgraphDefaultMinDepth = 0

backlinksDefaultMaxDepth = 2

-- By default, do not include the target file.
backlinksDefaultMinDepth = 1

data BacklinkOptions = BacklinkOptions
  { blTargets :: [SubgraphTarget],
    blMaxDepth :: Integer,
    blMinDepth :: Integer
  }
  deriving (Show)
