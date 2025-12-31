{-# LANGUAGE DeriveGeneric #-}

module MdGraph.File.Types where

import Data.Hashable (Hashable)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

type DefaultExtension = FilePath

type FileExtension = FilePath

type SourceFilePath = FilePath

type DestFilePath = FilePath

-- | Paths relative to the library directory
newtype RelativePath = RelativePath {unRelativePath :: FilePath}
  deriving (Show, Ord, Eq, Generic)

instance Hashable RelativePath

-- | Paths absolute to the filesystem
newtype AbsolutePath = AbsolutePath {unAbsolutePath :: FilePath}
  deriving (Show, Ord, Eq, Generic)

class HasPath a where
  getPath :: a -> FilePath

instance HasPath AbsolutePath where
  getPath = unAbsolutePath

instance Hashable AbsolutePath

data File = File
  { absolutePath :: AbsolutePath,
    modificationTime :: UTCTime
  }
  deriving (Show, Eq, Ord)
