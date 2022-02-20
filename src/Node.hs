{-# LANGUAGE DeriveGeneric #-}
module Node where

import           Data.Hashable
import           Data.Text                     as T
import           GHC.Generics                   ( Generic )

data Node = Link { linkPath :: FilePath } | Tag { tagLabel :: Text }
    deriving (Eq, Generic)

instance Show Node where
    show (Link l) = l
    show (Tag  t) = T.unpack t

instance Hashable Node where

isLink (Link l) = True
isLink _        = False

isTag = not . isLink

nodePath :: Node -> Maybe FilePath
nodePath (Link l) = Just l
nodePath _        = Nothing

tagText :: Node -> Maybe Text
tagText (Tag t) = Just t
tagText _       = Nothing
