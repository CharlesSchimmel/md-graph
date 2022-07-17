{-# LANGUAGE DeriveGeneric #-}

module MdGraph.Node where

import           MdGraph.File                   ( fixLink )

import           Data.Hashable
import           Data.Text                     as T
import           Data.Time                      ( UTCTime )
import           GHC.Generics                   ( Generic )

data Link = Link
    { linkPath :: FilePath
    , linkText :: Text
    }
    deriving (Eq, Generic, Show)

instance Hashable Link

data Tag = Tag
    { tagLabel :: Text
    }
    deriving (Eq, Generic, Show)

instance Hashable Tag
