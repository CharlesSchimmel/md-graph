{-# LANGUAGE DeriveGeneric #-}
module Node where

import           Data.Hashable
import           Data.Text                     as T
import           GHC.Generics                   ( Generic )

data Node = Link { linkPath :: FilePath } | Tag { tagText :: Text }
    deriving (Show, Eq, Generic)

instance Hashable Node where

isLink (Link l) = True
isLink _        = False

isTag = not . isLink

nodePath :: Node -> Maybe FilePath
nodePath (Link l) = Just l
nodePath _        = Nothing
