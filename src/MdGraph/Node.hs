{-# LANGUAGE DeriveGeneric #-}

module MdGraph.Node where

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

-- | A link with an absolute filepath
newtype AbsoluteLink = AbsoluteLink { unAbsoluteLink :: Link }
    deriving Show

-- | A link with a filepath relative to the library
newtype RelativeLink = RelativeLink { unRelativeLink :: Link }
    deriving Show

